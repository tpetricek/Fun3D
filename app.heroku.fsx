// --------------------------------------------------------------------------------------
// Start the 'app' WebPart defined in 'app.fsx' on Heroku using %PORT%
// --------------------------------------------------------------------------------------

printfn "loading app"
#load "app.fsx"
open App
open System
open Suave

printfn "running server"

let serverConfig =
  let port = int (Environment.GetEnvironmentVariable("PORT"))
  { Web.defaultConfig with
      homeFolder = Some __SOURCE_DIRECTORY__
      logger = Logging.Loggers.saneDefaultsFor Logging.LogLevel.Warn
      bindings = [ Types.HttpBinding.mk' Types.HTTP "0.0.0.0" port ] }

Web.startWebServer serverConfig app
