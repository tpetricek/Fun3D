#r "../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
open FSharp.Data

let shouldContain what (input:string) = 
  if not (input.Contains(what)) then failwith "shouldContain"
let shouldFail f = 
  if (try ignore(f()); true with _ -> false) then failwith "shouldFail"

let source1 = 
  "Fun.cube |> Fun."
let source2 = 
  "Fun.cone $\n" +
  "( Fun.cylinder|> Fun.move (0.0, 1.0, 0.0) )"

let checks count = async {
  for i in 0 .. count do
    do! Async.Sleep(100)
    Http.RequestString("http://localhost:8080/check", body=TextRequest source1)
    |> shouldContain "15"
    do! Async.Sleep(100)
    Http.RequestString("http://localhost:8080/check", body=TextRequest source2)
    |> shouldContain ": []" }

let declarations count = async {
  for i in 0 .. count do
    do! Async.Sleep(100)
    Http.RequestString("http://localhost:8080/declarations?line=1&col=16", body=TextRequest source1)
    |> shouldContain "val sphere"
    do! Async.Sleep(100)
    Http.RequestString("http://localhost:8080/declarations?line=2&col=2", body=TextRequest source2)
    |> shouldContain "type unit = Unit" }

let runs count = async {
  for i in 0 .. count do
    do! Async.Sleep(100)
    (fun () -> Http.RequestString("http://localhost:8080/run", body=TextRequest source1))
    |> shouldFail
    do! Async.Sleep(100)  
    Http.RequestString("http://localhost:8080/run", body=TextRequest source2)
    |> shouldContain "Fun__cylinder" }

runs 100 |> Async.RunSynchronously

Async.Parallel
  [ 
    checks 100
    declarations 100
    runs 100 
    runs 100 
  ]
|> Async.RunSynchronously