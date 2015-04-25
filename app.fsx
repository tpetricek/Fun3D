#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "packages/Suave/lib/net40/Suave.dll"
#r "packages/FSharp.Compiler.Service/lib/net40/FSharp.Compiler.Service.dll"

open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Interactive.Shell
open System.Text
open System.IO

// ------------------------------------------------------------------------------------------------
// Compiler service wrapper
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

let formatComment cmt (sb:StringBuilder) =
    match cmt with
    | FSharpXmlDoc.Text(s) -> sb.AppendLine(s) |> ignore
    | FSharpXmlDoc.XmlDocFileSignature(file, signature) -> ()
    | FSharpXmlDoc.None -> ()

let formatTipElement isSingle el (sb:StringBuilder) =
    match el with
    | FSharpToolTipElement.None -> ()
    | FSharpToolTipElement.Single(it, comment) -> sb.AppendLine(it) |> formatComment comment
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


let checker = FSharpChecker.Create()

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

// Line & col are 1-based
let getDeclarations (fileName, source) (line, col) = async {
    let! parse, check = checkFile (fileName, source)
    let textLine = getLines(source).[line-1]
    let _, _, names = extractNames textLine col 1
    let! decls = check.GetDeclarationListInfo(Some parse, line, col, textLine, names, "")
    return [ for it in decls.Items -> it.Name, it.Glyph, formatTip it.DescriptionText ] }

let getMethodOverloads (fileName, source) (line, col) = async {
    let! parse, check = checkFile (fileName, source)
    let textLine = getLines(source).[line-1]
    match extractNames textLine col 0 with
    | _, _, [] -> return List.empty
    | _, _, names ->
        let! methods = check.GetMethodsAlternate(line, col, textLine, Some names)
        return [ for m in methods.Methods -> formatTip m.Description ] }

// ------------------------------------------------------------------------------------------------
// Evaluator
// ------------------------------------------------------------------------------------------------

type FsiSession =
  { Session : FsiEvaluationSession
    ErrorString : StringBuilder }

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

let evalFunScript { Session = fsiSession; ErrorString = sbErr } (code:string) =
  try
    let code = getLines(code) |> Seq.map (fun s -> "  " + s) |> String.concat "\n"
    fsiSession.EvalInteraction("[<ReflectedDefinition>]\n" + "module Script =\n" + code)
    let entry = "FunScript.Compiler.Compiler.Compile(<@ Fun3D.Fun.show(Script.main()) @>)"
    match fsiSession.EvalExpression(entry) with
    | Some value -> value.ReflectionValue.ToString()
    | None -> failwith "Evaluation failed."
  with e -> failwithf "Evaluation failed: %O\nReported errors:\n%s" e (sbErr.ToString())

// ------------------------------------------------------------------------------------------------
// Server
// ------------------------------------------------------------------------------------------------

open System
open Suave
open Suave.Web
open Suave.Http
open Suave.Types
open FSharp.Data

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

let serviceHandler fsi scriptFile : WebPart = fun ctx -> async {
  match ctx.request.url.LocalPath, getRequestParams ctx with
  | "/run", (_, _, source) ->
      let jscode = evalFunScript fsi source
      return! ctx |> Successful.OK(jscode)

  | "/check", (_, _, source) ->
      let! _, check = checkFile (scriptFile, source)
      let res =
        [| for err in check.Errors ->
            JsonTypes.Error
              ( err.StartLineAlternate-1, err.StartColumn,
                err.EndLineAlternate-1, err.EndColumn, err.Message ) |]
      return! ctx |> Successful.OK (JsonTypes.Errors(res).JsonValue.ToString())

  | "/methods", (Some line, Some col, source) ->
      printfn "Methods: %A\n%s" (line, col) source
      let! meths = getMethodOverloads (scriptFile, source) (line, col)
      let res = JsonTypes.Methods(Array.ofSeq meths)
      return! ctx |> Successful.OK (res.JsonValue.ToString())

  | "/declarations", (Some line, Some col, source) ->
      printfn "Declarations: %A\n%s" (line, col) source
      let! decls = getDeclarations (scriptFile, source) (line, col)
      let res = [| for name, glyph, info in decls -> JsonTypes.Declaration(name, glyph, info) |]
      return! ctx |> Successful.OK (JsonTypes.Declarations(res).JsonValue.ToString())

  | _ ->
      return! Files.browseHome ctx }

let homeFolder = Path.Combine(__SOURCE_DIRECTORY__, "web")
let funFolder = Path.Combine(__SOURCE_DIRECTORY__, "funscript")
let scriptFile = Path.Combine(__SOURCE_DIRECTORY__, "funscript/script.fsx")

let fsi = startSession funFolder "#load \"Fun3D.fsx\""

let app =
  Writers.setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
  >>= Writers.setHeader "Pragma" "no-cache"
  >>= Writers.setHeader "Expires" "0"
  >>= serviceHandler fsi scriptFile

let serverConfig =
  let port = System.Environment.GetEnvironmentVariable("PORT")
  { defaultConfig with
      homeFolder = Some homeFolder
      logger = Logging.Loggers.saneDefaultsFor Logging.LogLevel.Verbose
      bindings=[ ( if port = null then HttpBinding.mk' HTTP  "127.0.0.1" 8080
                   else HttpBinding.mk' HTTP  "0.0.0.0" (int port) ) ] }

#if START_SERVER
startWebServer serverConfig app
#endif

// let _, server = startWebServerAsync serverConfig app
// let cts = new System.Threading.CancellationTokenSource()
// Async.Start(server, cts.Token)
// cts.Cancel()
