#load "app.fsx"

#r "packages/FAKE/tools/FakeLib.dll"
open Fake
open Suave
open Suave.Web
open Suave.Http
open Suave.Types

Target "run" (fun _ ->
  let serverConfig =
    { defaultConfig with
        homeFolder = Some __SOURCE_DIRECTORY__
        logger = Logging.Loggers.saneDefaultsFor Logging.LogLevel.Warn
        bindings = [ HttpBinding.mk' HTTP  "127.0.0.1" 80 ] }
  startWebServer serverConfig App.app
)

RunTargetOrDefault "run"