// --------------------------------------------------------------------------------------
// A simple FAKE build script that hosts Suave server and 
// automatically reloads web part that is defined in 'app.fsx'.
//
// This script uses FileSystemWatcher to look for changes in 'app.fsx'. When
// the file changes, we run `#load "app.fsx"` using the F# Interactive service
// and then get the `App.app` value (top-level value defined using `let app = ...`).
// The loaded WebPart is then hosted at localhost:8080.
// --------------------------------------------------------------------------------------

#r "packages/Suave/lib/net40/Suave.dll"
#r "packages/FAKE/tools/FakeLib.dll"
#r "packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"

open Fake
open System
open System.IO
open Suave
open Suave.Web
open Suave.Types
open Microsoft.FSharp.Compiler.Interactive.Shell

// --------------------------------------------------------------------------------------
// Using F# compiler service to load the 'app' value from 'app.fsx'
// --------------------------------------------------------------------------------------

let sbOut = new Text.StringBuilder()
let sbErr = new Text.StringBuilder()

let fsiSession = 
  let inStream = new StringReader("")
  let outStream = new StringWriter(sbOut)
  let errStream = new StringWriter(sbErr)
  let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
  let argv = Array.append [|"C:\\test.exe"; "--quiet"; "--noninteractive"|] [||]
  FsiEvaluationSession.Create(fsiConfig, argv, inStream, outStream, errStream)

let reportFsiError (e:exn) =
  traceError "Reloading app.fsx script failed."
  traceError (sprintf "Message: %s\nError: %s" e.Message (sbErr.ToString().Trim()))
  sbErr.Clear() |> ignore

let reloadScript () = 
  try
    traceImportant "Reloading app.fsx script..."
    let appFsx = __SOURCE_DIRECTORY__ @@ "app.fsx"
    fsiSession.EvalInteraction(sprintf "#load @\"%s\"" appFsx)
    fsiSession.EvalInteraction("open App")
    match fsiSession.EvalExpression("app") with
    | Some app -> Some(app.ReflectionValue :?> WebPart)
    | None -> failwith "Couldn't get 'app' value"
  with e -> reportFsiError e; None

// --------------------------------------------------------------------------------------
// Using FileSystemWatcher to reload automatically when 'app.fsx' changes
// --------------------------------------------------------------------------------------

let currentApp = ref (fun _ -> async { return None })

let handleWatcherEvents e =
  reloadScript() |> Option.iter (fun app -> 
    currentApp.Value <- app
    traceImportant "New version of app.fsx loaded!" )

let watchForChanges () =
  use watcher = new System.IO.FileSystemWatcher(__SOURCE_DIRECTORY__, "app.fsx")
  watcher.EnableRaisingEvents <- true
  watcher.IncludeSubdirectories <- true
  watcher.Changed.Add(handleWatcherEvents)
  watcher.Created.Add(handleWatcherEvents)
  watcher.Renamed.Add(handleWatcherEvents)
  traceImportant "Waiting for app.fsx edits. Press any key to stop."
  System.Console.ReadLine() |> ignore

// --------------------------------------------------------------------------------------
// Suave server that redirects all request to currently loaded version
// --------------------------------------------------------------------------------------

let serverConfig =
  { defaultConfig with
      homeFolder = Some __SOURCE_DIRECTORY__
      logger = Logging.Loggers.saneDefaultsFor Logging.LogLevel.Debug
      bindings = [ HttpBinding.mk' HTTP  "127.0.0.1" 8080] }

Target "local" (fun _ ->
  let app ctx = currentApp.Value ctx
  let _, server = startWebServerAsync serverConfig app
  let cts = new System.Threading.CancellationTokenSource()

  // Load the server in `app.fsx`
  handleWatcherEvents ()
  // Start Suave to host it on localhost
  Async.Start(server, cts.Token)
  // Open web browser with the loaded file
  System.Diagnostics.Process.Start("http://localhost:8080") |> ignore

  watchForChanges()
  cts.Cancel() |> ignore
)

Target "build" (fun _ ->
    !! (__SOURCE_DIRECTORY__ @@ "Fun3D.sln")
    |> MSBuildRelease "" "Rebuild"
    |> Log "AppBuild-Output: "
)

Target "run" (fun _ ->
  let info = new System.Diagnostics.ProcessStartInfo(__SOURCE_DIRECTORY__ @@ "bin/Release/Fun3D.exe")
  info.UseShellExecute <- false
  info.WorkingDirectory <- __SOURCE_DIRECTORY__ @@ "bin/Release"

  // Run the process and wait for it to complete
  let proc = System.Diagnostics.Process.Start(info)
  proc.WaitForExit()
)

"build" ==> "run"

RunTargetOrDefault "local"