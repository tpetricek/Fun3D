#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "packages/Suave/lib/net40/Suave.dll"
#r "packages/FSharp.Compiler.Service/lib/net40/FSharp.Compiler.Service.dll"

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Interactive.Shell
open Microsoft.FSharp.Compiler.Ast
open System.Text
open System.IO

// ------------------------------------------------------------------------------------------------
// F# compiler service wrapper
// ------------------------------------------------------------------------------------------------

/// Split the input string into an array of lines (using \r\n or \n as separator)
let getLines (s:string) = s.Replace("\r\n", "\n").Split('\n')

/// Extracts all consecutive identifiers to the left of the charIndex for a specified line of code
let extractIdentTokens line charIndex =
    let sourceTok = SourceTokenizer([], "/home/test.fsx")
    let tokenizer = sourceTok.CreateLineTokenizer(line)

    let rec gatherTokens (tokenizer:FSharpLineTokenizer) state = seq {
      match tokenizer.ScanToken(state) with
      | Some tok, state ->
          yield tok
          yield! gatherTokens tokenizer state
      | None, state -> () }

    let tokens = gatherTokens tokenizer 0L |> Seq.toArray
    let idx = tokens |> Array.tryFindIndex(fun x ->
      charIndex > x.LeftColumn && charIndex <= x.LeftColumn + x.FullMatchedLength)

    match idx with
    | Some(endIndex) ->
        let startIndex =
            tokens.[0..endIndex]
            |> Array.rev
            |> Array.tryFindIndex (fun x -> x.TokenName <> "IDENT" && x.TokenName <> "DOT")
            |> Option.map (fun x -> endIndex - x)
        let startIndex = defaultArg startIndex 0
        let idents = tokens.[startIndex..endIndex] |> Array.filter (fun x -> x.TokenName = "IDENT")
        Some tokens.[endIndex], idents

    | None -> None, Array.empty

/// Parses the line of F# code and builds a list of identifier names in order
/// to be passed into the `GetDeclarations`, `GetMethods`, or other functions
///
/// For tooltips and overlodas, set identOffset=0; For completion set identOffset=1
let extractNames line charIndex identOffset =
    let charToken, tokens = extractIdentTokens line charIndex
    match charToken with
    | None -> 0, 0, []
    | Some(charToken) ->
        let names = tokens |> Array.map (fun x ->
          line.Substring(x.LeftColumn, x.FullMatchedLength).Trim('`'))
        let takeSize = tokens.Length - identOffset
        let finalList =
          if charToken.TokenName = "IDENT" && Array.length(tokens) > takeSize then
            names |> Seq.take takeSize |> Seq.toList
          else
            names |> Seq.toList
        (charToken.LeftColumn, charToken.LeftColumn + charToken.FullMatchedLength, finalList)

// Mostly boring code to format tooltips reported from method overloads
let htmlEncode (s:string) = s.Trim().Replace("&", "&amp;").Replace("<", "&lt;").Replace(">", "&gt;")
let formatComment cmt (sb:StringBuilder) =
    match cmt with
    | FSharpXmlDoc.Text(s) -> sb.AppendLine(sprintf "<p class=\"cmt\">%s</p>" (s.Trim())) |> ignore
    | FSharpXmlDoc.XmlDocFileSignature(file, signature) -> ()
    | FSharpXmlDoc.None -> ()
let formatTipElement isSingle el (sb:StringBuilder) =
    match el with
    | FSharpToolTipElement.None -> ()
    | FSharpToolTipElement.Single(it, comment) -> 
        sb.AppendLine(sprintf "<p class=\"sig\">%s</p>" (htmlEncode it)) 
        |> formatComment comment
    | FSharpToolTipElement.Group(items) ->
        let items, msg =
          if items.Length > 10 then
            (items |> Seq.take 10 |> List.ofSeq),
            sprintf "   (+%d other overloads)" (items.Length - 10)
          else items, ""
        if isSingle && items.Length > 1 then
          sb.AppendLine("Multiple overloads") |> ignore
        for (it, comment) in items do
          sb.AppendLine(it) |> formatComment comment
        if msg <> null then sb.AppendFormat(msg) |> ignore
    | FSharpToolTipElement.CompositionError(err) ->
        sb.Append("Composition error: " + err) |> ignore
let formatTip tip =
  let sb = StringBuilder()
  match tip with
  | FSharpToolTipText([single]) -> formatTipElement true single sb
  | FSharpToolTipText(its) -> for item in its do formatTipElement false item sb
  sb.ToString().Trim('\n', '\r')

/// Running F# interactive checker for doing all the things!
/// (NOTE: I guess it might be good idea to restart this every now and then)
let checker = FSharpChecker.Create()

/// Check specified file and return parsing & type checking results
let checkFile (fileName, source) = async {
    let! options = checker.GetProjectOptionsFromScript(fileName, source)
    match checker.TryGetRecentTypeCheckResultsForFile(fileName, options, source) with
    | Some(parse, check, _) -> return parse, check
    | None ->
        let! parse = checker.ParseFileInProject(fileName, source, options)
        let! answer = checker.CheckFileInProject(parse, fileName, 0, source, options)
        match answer with
        | FSharpCheckFileAnswer.Succeeded(check) -> return parse, check
        | FSharpCheckFileAnswer.Aborted -> return failwith "Parsing did not finish" }

/// Get declarations (completion) at the specified line & column (lines are 1-based)
let getDeclarations (fileName, source) (line, col) = async {
    let! parse, check = checkFile (fileName, source)
    let textLine = getLines(source).[line-1]
    let _, _, names = extractNames textLine col 1
    let! decls = check.GetDeclarationListInfo(Some parse, line, col, textLine, names, "")
    return [ for it in decls.Items -> it.Name, it.Glyph, formatTip it.DescriptionText ] }

/// Get method overloads (for the method before '('). Lines are 1-based
let getMethodOverloads (fileName, source) (line, col) = async {
    let! parse, check = checkFile (fileName, source)
    let textLine = getLines(source).[line-1]
    match extractNames textLine col 0 with
    | _, _, [] -> return List.empty
    | _, _, names ->
        let! methods = check.GetMethodsAlternate(line, col, textLine, Some names)
        return [ for m in methods.Methods -> formatTip m.Description ] }

// ------------------------------------------------------------------------------------------------
// FunScript + F# Compiler Service Evaluator
// ------------------------------------------------------------------------------------------------

type FsiSession =
  { Session : FsiEvaluationSession
    ErrorString : StringBuilder }

/// Start F# Interactive, reference all assemblies in `refFolder` 
/// evaluate the initial `loadScript` and return running 'FsiSession'
let startSession refFolder loadScript =
  let sbOut = new StringBuilder()
  let sbErr = new StringBuilder()
  let inStream = new StringReader("")
  let outStream = new StringWriter(sbOut)
  let errStream = new StringWriter(sbErr)

  // Start the F# Interactive service process
  let refFiles = Directory.GetFiles(refFolder, "*.dll")
  let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
  let fsiSession =
    FsiEvaluationSession.Create
      ( fsiConfig, [| "/temp/fsi.exe"; "--noninteractive" |],
        inStream, outStream, errStream )

  // Load referenced libraries & run initialization script
  try
    fsiSession.EvalInteraction(sprintf "#I @\"%s\"" refFolder)
    for lib in refFiles do fsiSession.EvalInteraction(sprintf "#r @\"%s\"" lib)
    fsiSession.EvalInteraction(loadScript)
    { Session = fsiSession; ErrorString = sbErr }
  with _ -> failwithf "F# Interactive initialization failed: %s" (sbErr.ToString())


/// Check that the user didn't do anything to escape quoted expression
/// (i.e. they are not trying to run any code on our server..)
let checkScriptStructure (scriptFile, source) = async {
  let! options = checker.GetProjectOptionsFromScript(scriptFile, source)
  let! parsed = checker.ParseFileInProject(scriptFile, source, options)
  match parsed.ParseTree with
  | Some tree -> 
      match tree with
      // Expecting: single file containing single module named "Script"
      | ParsedInput.ImplFile
          (ParsedImplFileInput(_,_,_,_,_,[SynModuleOrNamespace([id],_,decls,_,_,_,_)],_)) 
            when id.idText = "Script" ->
        match decls with 
        // Expecting: FunScript.Compiler.Compiler.Compile(<@ .. @>)
        // (if all user code is inside quotation, it does not get run)
        | [ SynModuleDecl.DoExpr
              (_, SynExpr.App
                    ( _, _, SynExpr.LongIdent _, 
                      SynExpr.Paren(SynExpr.Quote _, _, _, _), _), _) ] -> ()
        | _ -> failwith "Unexpected AST!"
      | _ -> failwith "Unexpected AST!"
  | _ -> failwith "Could not parse the specified AST" }

/// Pass the specified code to FunScript and return JavaScript that we'll
/// send back to the client (so that they can run it themselves)
let evalFunScript { Session = fsiSession; ErrorString = sbErr } (scriptFile, code) = async {
  let allCode = 
    [ yield "FunScript.Compiler.Compiler.Compile(<@"
      for line in getLines code do yield "  " + line
      yield "  |> Fun3D.Fun.show"
      yield "@>)" ] 
    |> String.concat "\n"
  do! checkScriptStructure (scriptFile, allCode)

  try
    match fsiSession.EvalExpression(allCode) with
    | Some value -> return value.ReflectionValue.ToString()
    | None -> return failwith "Evaluation failed."
  with e -> 
    let errors = sbErr.ToString()
    sbErr.Clear() |> ignore
    return failwithf "Evaluation failed: %O\nReported errors:\n%s" e errors }

// ------------------------------------------------------------------------------------------------
// Sharing snippets on www.fssnip.net
// ------------------------------------------------------------------------------------------------

open FSharp.Data

/// JSON that we get from the JavaScript in the browser
type SnippetInfo = JsonProvider<"""{"title":"h","author":"h","info":"h",
  "public":true,"code":"Fun.cube $ Fun.cone"}""">

/// Request type that we want to send to www.fssnip.net
type FsSnipTypes = JsonProvider<"""{
  "request": { "title": "Hello", "author": "Tomas Petricek", "description": "Hello world", 
    "code": "1+2", "tags": [ "test" ], "public": true, "link": "http://tomasp.net", 
    "nugetpkgs": ["na"], "source": "fun3d" } }""">

let header = 
  "// This snippet has been shared from Fun3D web site.\n" +
  "// You can run it in your web browser at: www.fun3d.net!\n"

/// Sends the specified snippet ('SnippetInfo' format from the 
/// client) to the API exposed by www.fssnip.net.
let shareSnippet jsonText = async {
  let info = SnippetInfo.Parse(jsonText)
  let req =
    FsSnipTypes.Request
      ( info.Title, info.Author, info.Info, header + info.Code, [| "fun3d" |], info.Public,
        "http://www.fun3d.net", [| |], "fun3d")
  let! res = 
    Http.AsyncRequestString
      ( "http://api.fssnip.net/1/snippet", httpMethod="PUT", 
        body=HttpRequestBody.TextRequest (req.ToString()) )
  return res }

// ------------------------------------------------------------------------------------------------
// Suave.io web server 
// ------------------------------------------------------------------------------------------------

open System
open Suave
open Suave.Web
open Suave.Http
open Suave.Types

/// Types of JSON values that we are returning from F# Compiler Service calls
type JsonTypes = JsonProvider<"""{
    "declarations":
      {"declarations":[ {"name":"Method", "glyph":1, "documentation":"Text"} ]},
    "errors":
      {"errors":[ {"startLine":1, "startColumn":1, "endLine":1, "endColumn":1, "message":"error"} ]},
    "methods":
      {"methods":[ "first info", "second info" ] }
  }""">

/// Get parameters of an F# compiler service request. Returns
/// a tuple with line, column & source (the first two may be optional)
let getRequestParams (ctx:HttpContext) =
  use sr = new StreamReader(new MemoryStream(ctx.request.rawForm))
  let tryAsInt s = match Int32.TryParse s with true, n -> Some n | _ -> None
  ctx.request.queryParam "line" |> Option.bind tryAsInt,
  ctx.request.queryParam "col" |> Option.bind tryAsInt,
  sr.ReadToEnd()

// This script is implicitly inserted before every source code we get
let loadScript = 
  [| "#load \"Fun3D.fsx\"\n"
     "open Fun3D\n" |]
let loadScriptString = 
  String.Concat(loadScript)

let noCacheSuccess res = 
  Writers.setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
  >>= Writers.setHeader "Pragma" "no-cache"
  >>= Writers.setHeader "Expires" "0"
  >>= Successful.OK(res)

/// The main handler for Suave server!
let serviceHandler fsi scriptFile : WebPart = fun ctx -> async {
  match ctx.request.url.LocalPath, getRequestParams ctx with
  
  // Transform F# `source` into JavaScript and return it
  | "/run", (_, _, source) ->
      let! jscode = evalFunScript fsi (scriptFile, source)
      return! ctx |> noCacheSuccess jscode

  // Share the snippet on www.fssnip.net and return its URL & details
  | "/share", (_, _, json) ->
      let! res = shareSnippet json
      return! ctx |> noCacheSuccess res

  // Type-check the source code & return list with error information
  | "/check", (_, _, source) ->
      let! _, check = checkFile (scriptFile, loadScriptString + source)
      let res =
        [| for err in check.Errors ->
            JsonTypes.Error
              ( err.StartLineAlternate-1-loadScript.Length, err.StartColumn,
                err.EndLineAlternate-1-loadScript.Length, err.EndColumn, err.Message ) |]
      return! ctx |> noCacheSuccess (JsonTypes.Errors(res).JsonValue.ToString())

  // Get method overloads & parameter info at the specified location in the source
  | "/methods", (Some line, Some col, source) ->
      let! meths = 
        getMethodOverloads (scriptFile, loadScriptString + source) 
                           (line + loadScript.Length, col)
      let res = JsonTypes.Methods(Array.ofSeq meths)
      return! ctx |> noCacheSuccess (res.JsonValue.ToString())

  // Get auto-completion for the specified location
  | "/declarations", (Some line, Some col, source) ->
      let! decls = 
        getDeclarations (scriptFile, loadScriptString + source) 
                        (line + loadScript.Length, col)
      let res = [| for name, glyph, info in decls -> JsonTypes.Declaration(name, glyph, info) |]
      return! ctx |> noCacheSuccess (JsonTypes.Declarations(res).JsonValue.ToString())

  // Otherwise, just serve the files from 'web' using 'index.html' as default
  | _ ->
      let webDir = Path.Combine(ctx.runtime.homeDirectory, "web")
      let subRuntime = { ctx.runtime with homeDirectory = webDir }
      let webPart = 
        if ctx.request.url.LocalPath <> "/" then Files.browseHome
        else Files.browseFileHome "index.html"
      return! webPart { ctx with runtime = subRuntime } }

// ------------------------------------------------------------------------------------------------
// Start the server (on Heroku) or expose 'app' value for 'host.fsx' local runner
// ------------------------------------------------------------------------------------------------

// Directory with FunScript binaries and 'Fun3D.fsx'
let funFolder = Path.Combine(__SOURCE_DIRECTORY__, "funscript")
let scriptFile = Path.Combine(__SOURCE_DIRECTORY__, "funscript/script.fsx")
let fsi = startSession funFolder loadScriptString
let app = serviceHandler fsi scriptFile

#if START_SERVER
let serverConfig =
  let port = System.Environment.GetEnvironmentVariable("PORT")
  { defaultConfig with
      homeFolder = Some __SOURCE_DIRECTORY__
      logger = Logging.Loggers.saneDefaultsFor Logging.LogLevel.Warn
      bindings=[ (HttpBinding.mk' HTTP  "0.0.0.0" (int port) ) ] }
startWebServer serverConfig app
#endif