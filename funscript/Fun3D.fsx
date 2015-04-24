[<ReflectedDefinition>]
module Fun3D

#if INTERACTIVE
#r "FunScript.dll"
#r "FunScript.Interop.dll"
#r "FunScript.TypeScript.Binding.lib.dll"
#r "FunScript.TypeScript.Binding.three.dll"
#endif

open System
open FunScript
open FunScript.TypeScript

// --------------------------------------------------------------------------------------
// JavaScript and Three.js helpers
// --------------------------------------------------------------------------------------

module Internal = 
  [<JSEmit("return { color: {0} };")>]
  let colorObj (s:obj) : obj = failwith "never"

  [<JSEmit("return {0} == null;")>]
  let isNull (o:obj) : bool = failwith "never"

  [<JSEmit("return new THREE.Scene();")>]
  let ThreeScene () : THREE.Scene = failwith "never"

  [<JSEmit("return new THREE.SpotLight({0});")>]
  let ThreeSpotLight (color:int) : THREE.SpotLight = failwith "never"

  [<JSEmit("return THREE.LinearFilter;")>]
  let ThreeLinearFilter () : THREE.TextureFilter = failwith "never"

  [<JSEmit("navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia;\n" +
           "navigator.getUserMedia({video: true}, {0}, {1});")>]
  let getVideoMedia gotStream noStream : unit = failwith "never"

  [<JSEmit("return requestAnimationFrame({0});")>]
  let requestAnimationFrame(f:obj) : unit = failwith "never"

  [<JSEmit("return { ambient: 0x030303, color: {0}, specular: 0x606060, shininess: 10, shading: THREE.FlatShading }")>]
  let fancyMaterial (color:int) : obj = failwith "never"

  [<JSEmit("return { map: {0}, overdraw: true, side:THREE.DoubleSide }")>]
  let cameraMaterial (map:obj) : obj = failwith "never"

  let setVariable (name:string) value =
    (Globals.eval("(function(x){document." + name + " = x;})") |> unbox<obj -> unit>) value

// --------------------------------------------------------------------------------------
// Domain-specific langauge for composing 3D
// --------------------------------------------------------------------------------------
open Internal

type Point3D = { X : float; Y : float; Z : float }

type Material = 
  { GetMaterial : unit -> THREE.Material }

type Context =
  { Parent : THREE.Object3D
    Material : THREE.Material 
    Position : Point3D
    Rotation : Point3D }

type Fun3D = 
  { Render : Context -> unit }

let ($) (scene1:Fun3D) (scene2:Fun3D) = 
  { Render = fun ctx ->
      scene1.Render(ctx)
      scene2.Render(ctx) }

module Mat = 
  let movie = 
    { GetMaterial = fun () -> Globals.eval("document.fun3dMovieMaterial") |> unbox }

  let flat color = 
    { GetMaterial = fun () -> THREE.MeshBasicMaterial.Create(unbox (colorObj color)) :> _ } 

module Color =
  let red = 0xff0000
  let green = 0x00ff00
  let blue = 0x0000ff
  let yellow = 0xffff00
  let purple = 0xff00ff
  let cyan = 0x00ffff
  let orange = 0xffa500

module Fun = 
  let private f3d f = { Render = f }

  let cube = f3d <| fun ctx ->
    let geometry = THREE.BoxGeometry.Create( 1., 1., 1. )
    let cube = THREE.Mesh.Create( geometry, ctx.Material )
    ctx.Parent.add(cube) 

  let cylinder = f3d <| fun ctx ->
    let geometry = THREE.CylinderGeometry.Create( 0.5, 0.5, 1., 32., 1. )
    let cylinder = THREE.Mesh.Create( geometry, ctx.Material )
    ctx.Parent.add( cylinder )

  let cone = f3d <| fun ctx ->
    let geometry = THREE.CylinderGeometry.Create( 0., 0.5, 1., 32., 1. )
    let cone = THREE.Mesh.Create( geometry, ctx.Material )
    ctx.Parent.add( cone )

  let sphere = f3d <| fun ctx ->
    let geometry = THREE.SphereGeometry.Create( 1., 256., 256. )
    let cone = THREE.Mesh.Create( geometry, ctx.Material )
    ctx.Parent.add( cone )

  let text message = f3d <| fun ctx ->
    let geometry = THREE.TextGeometry.Create(message)
    let cone = THREE.Mesh.Create( geometry, ctx.Material )
    ctx.Parent.add( cone )

  let color color (scene:Fun3D) = f3d  <| fun ctx -> 
    scene.Render { ctx with Material = THREE.MeshPhongMaterial.Create(unbox (fancyMaterial color)) } 

  let material (material:Material) (scene:Fun3D) = f3d  <| fun ctx -> 
    scene.Render { ctx with Material = material.GetMaterial() } 

  let scale (sx,sy,sz) (scene:Fun3D) = f3d <| fun ctx ->
    let root = THREE.Object3D.Create()
    root.scale.set(sx,sy,sz) |> ignore
    scene.Render { ctx with Parent = root }
    ctx.Parent.add(root)

  let move (dx,dy,dz) (scene:Fun3D) = f3d <| fun ctx ->
    let root = THREE.Object3D.Create()
    root.translateX(dx)
    root.translateY(dy)
    root.translateZ(dz)
    scene.Render { ctx with Parent = root }
    ctx.Parent.add(root)

  let rotate (rx,ry,rz) (scene:Fun3D) = f3d <| fun ctx ->
    let root = THREE.Object3D.Create()
    root.rotation.set
      ( rx/180.0*System.Math.PI,
        ry/180.0*System.Math.PI,
        rz/180.0*System.Math.PI) |> ignore
    scene.Render { ctx with Parent = root }
    ctx.Parent.add(root)

  /// ASSUMPTIONS: '#first' element
  let show (it:Fun3D) =

    // Scene setup - run this only once
    let setup () =
      let first = Globals.document.getElementById("first")
      let width, height = first.offsetWidth, first.offsetHeight
      let camera = THREE.PerspectiveCamera.Create(75., width / height, 0.1, 1000.0 )
      let renderer = THREE.WebGLRenderer.Create()
      renderer.setSize( width, height )
      Globals.document.getElementById("first").appendChild( renderer.domElement ) |> ignore
      camera.position.z <- 8.0
      camera.position.y <- 0.75

      let scene = ThreeScene()
      let spotLight = ThreeSpotLight(unbox 0xffffff )
      spotLight.position.set( 25., 25., 50. ) |> ignore
      spotLight.castShadow <- true
      spotLight.shadowMapWidth <- 1024.
      spotLight.shadowMapHeight <- 1024.
      spotLight.shadowCameraNear <- 500.
      spotLight.shadowCameraFar <- 4000.
      spotLight.shadowCameraFov <- 30.
      scene.add( spotLight )

      let root = THREE.Object3D.Create()
      scene.add(root)

      // -------- video stuff
      Internal.getVideoMedia
        (fun s -> Globals.alert("got stream!"))
        (fun e -> Globals.alert("no stream!"))
(*
      
  navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia;
  window.URL = window.URL || window.webkitURL;

  var camvideo = document.getElementById('monitor');

  function gotStream(stream) {
    alert('yo stream');
    if (window.URL)
    { camvideo.src = window.URL.createObjectURL(stream); }
    else // Opera
    { camvideo.src = stream; }

    camvideo.onerror = function (e)
    { stream.stop(); };

    stream.onended = noStream;
  }

  function noStream(e) {
    alert('no stream: ' + e.name);
    var msg = 'No camera available.';
    if (e.code == 1)
    { msg = 'User denied access to use camera.'; }
    //document.getElementById('errorMessage').textContent = msg;
  }

      if (!navigator.getUserMedia)
      {
          document.getElementById('errorMessage').innerHTML =
              'Sorry. <code>navigator.getUserMedia()</code> is not available.';
      } else {
          navigator.getUserMedia({video: true}, gotStream, noStream);
      }

*)

      let video = Globals.document.getElementById("monitor")
      let videoImage = Globals.document.getElementById("videoImage") |> unbox<HTMLCanvasElement>
      let videoImageContext = videoImage.getContext_2d() //("2d")
      videoImageContext.fillStyle <- "#000000"
      videoImageContext.fillRect( 0., 0., videoImage.width, videoImage.height )
  
      let videoTexture = THREE.Texture.Create(videoImage)
      videoTexture.minFilter <- ThreeLinearFilter()
      videoTexture.magFilter <- ThreeLinearFilter()
 
      let movieMaterial = THREE.MeshBasicMaterial.Create(cameraMaterial videoTexture |> unbox)
      // -------- end of video stuff

      let rec render (r) = 
        requestAnimationFrame(fun () -> render (r+0.01))
        if unbox video.readyState = (*video.HAVE_ENOUGH_DATA*) 4 then
          videoImageContext.drawImage( video, 0., 0., videoImage.width, videoImage.height )
          if not (isNull (videoTexture)) then
            videoTexture.needsUpdate <- true

        root.rotation.y <- r
        renderer.render(scene, camera)
      
      render 0.0

      setVariable "fun3dMovieMaterial" movieMaterial
      setVariable "fun3dCamera" camera
      setVariable "fun3dRoot" root

    // Call setup once
    if isNull(Globals.eval("document.fun3dCamera")) then setup()
    let camera = Globals.eval("document.fun3dCamera") |> unbox<THREE.PerspectiveCamera>
    let root = Globals.eval("document.fun3dRoot") |> unbox<THREE.Object3D>
    

    let ctx = 
      { Parent = root
        Material = THREE.MeshPhongMaterial.Create(unbox (fancyMaterial 0xffffff))
        Position = { X = 0.; Y = 0.; Z = 0. }
        Rotation = { X = 0.; Y = 0.; Z = 0. } }

    while root.children.length <> 0. do
      root.remove(root.children.[0])

    it.Render(ctx)

    Globals.document.addEventListener("keydown", fun e -> 
      let e = (unbox<FunScript.TypeScript.KeyboardEvent> e)
      let code = e.keyCode
      if code = 189. then
        camera.position.z <- camera.position.z + 0.3
        camera.position.y <- camera.position.y + 0.1
      elif code = 187. then
        camera.position.z <- camera.position.z - 0.3
        camera.position.y <- camera.position.y - 0.1 
      if e.getModifierState("Control") || e.getModifierState("Alt") then
        if code = 38. then
          camera.position.z <- camera.position.z - 0.1
          camera.position.y <- camera.position.y - 0.3
        elif code = 40. then
          camera.position.z <- camera.position.z + 0.1
          camera.position.y <- camera.position.y + 0.3 
        elif code = 37. then
          camera.position.x <- camera.position.x + 0.3
        elif code = 39. then
          camera.position.x <- camera.position.x - 0.3 )

open Fun

// --------------------------------------------------------------------------------------
// Block API
// --------------------------------------------------------------------------------------

type BlockKind = { Name : string }

module InternalBlock =
  [<JSEmit("return init(true);")>]
  let initBlocks () : unit = failwith "never"

  [<JSEmit("return block({0},{1},{2},{3});")>]
  let block (x:int,y:int,z:int,s:string) : unit = failwith "never"

module Block =
  open InternalBlock

  let crate = { Name = "crate" }   
  let video = { Name = "video" }   
  let grass = { Name = "grass" }   
  let brick = { Name = "brick" }   
  let water = { Name = "water" }   
  let stone = { Name = "stone" }   

  let block (x:int,y:int,z:int,kind:BlockKind) : unit =
    InternalBlock.block(x,y,z,kind.Name)

  let show (f) =
    initBlocks()
    f()