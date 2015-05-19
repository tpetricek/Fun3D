[<ReflectedDefinition>]
module Fun3D

#if INTERACTIVE
#r "FunScript.dll"
#r "FunScript.Interop.dll"
#r "FunScript.TypeScript.Binding.jquery.dll"
#r "FunScript.TypeScript.Binding.lib.dll"
#r "FunScript.TypeScript.Binding.three.dll"
#endif

open System
open FunScript
open FunScript.TypeScript

// --------------------------------------------------------------------------------------
// FunScript mappings for various JavaScript things and Three.js stuff
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

  [<JSEmit("navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia;\n" +
           "return (navigator.getUserMedia != null);")>]
  let canGetVideoMedia () : bool = failwith "never"

  [<JSEmit("window.URL = window.URL || window.webkitURL;\n" +
           "if (window.URL) { return window.URL.createObjectURL({0}); } else { return {0}; }")>]
  let getVideoStreamSource stream = failwith "never"

  [<JSEmit("return requestAnimationFrame({0});")>]
  let requestAnimationFrame(f:obj) : unit = failwith "never"

  [<JSEmit("return { ambient: 0x030303, color: {0}, specular: 0x606060, shininess: 10, shading: THREE.FlatShading }")>]
  let fancyMaterial (color:int) : obj = failwith "never"

  [<JSEmit("return { map: {0}, overdraw: true, side:THREE.DoubleSide }")>]
  let cameraMaterial (map:obj) : obj = failwith "never"

  let setVariable (name:string) value =
    (Globals.eval("(function(x){document." + name + " = x;})") |> unbox<obj -> unit>) value


// --------------------------------------------------------------------------------------
// Initialization code that loads camera & creates scene
// --------------------------------------------------------------------------------------

module Setup = 
  open Internal

  /// Function in a ref cell that is called from the rendering loop
  /// (this can be set to a function that updates the video stream)
  let renderVideo = ref ignore

  /// Try to capture camera and report errors if it fails..
  let setupVideo (rerender) =
    Globals.jQuery.Invoke("#alert-camera").addClass("in") |> ignore
    if Internal.canGetVideoMedia() then
      let camvideo = Globals.document.getElementById("monitor") :?> HTMLVideoElement
      Internal.getVideoMedia
        (fun stream -> 
            Globals.jQuery.Invoke("#alert-camera").removeClass("in") |> ignore
            camvideo.src <- getVideoStreamSource(stream) 
            rerender()
            Globals.window.setTimeout(rerender,1000)
        )
        (fun e -> 
            Globals.jQuery.Invoke("#alert-camera").removeClass("in") |> ignore
            Globals.jQuery.Invoke("#alert-camera-fail").addClass("in") |> ignore)
    else 
      Globals.jQuery.Invoke("#alert-camera").removeClass("in") |> ignore
      Globals.jQuery.Invoke("#alert-camera-na").addClass("in") |> ignore

    let video = Globals.document.getElementById("monitor")
    let videoImage = Globals.document.getElementById("videoImage") |> unbox<HTMLCanvasElement>
    let videoImageContext = videoImage.getContext_2d()
    videoImageContext.fillStyle <- "#000000"
    videoImageContext.fillRect(0., 0., videoImage.width, videoImage.height)
  
    let videoTexture = THREE.Texture.Create(videoImage)
    videoTexture.minFilter <- ThreeLinearFilter()
    videoTexture.magFilter <- ThreeLinearFilter()
 
    let movieMaterial = THREE.MeshBasicMaterial.Create(cameraMaterial videoTexture |> unbox)
    setVariable "fun3dMovieMaterial" movieMaterial

    // Register function that renders the video inside the loop
    renderVideo := fun () ->
      if unbox video.readyState = (*video.HAVE_ENOUGH_DATA*) 4 then
        videoImageContext.drawImage( video, 0., 0., videoImage.width, videoImage.height )
        if not (isNull (videoTexture)) then
          videoTexture.needsUpdate <- true

  /// Use + / - for zooming in and out; Use Ctrl + keys for moving view
  let setupKeyHandlers (camera:THREE.PerspectiveCamera) =
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

  /// Initialize everything on the scene (called just once) & start render loop
  let setupScene () =
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

    setVariable "fun3dRotation" 0.01
    setVariable "fun3dCamera" camera
    setVariable "fun3dRoot" root

    let rec render (r) = 
      let rotateBy = unbox<float> (Globals.eval("document.fun3dRotation"))
      requestAnimationFrame(fun () -> render (r + rotateBy))
      renderVideo.Value ()
      root.rotation.y <- r
      renderer.render(scene, camera)
      
    setupKeyHandlers camera
    render 0.0

// --------------------------------------------------------------------------------------
// Domain-specific langauge for composing 3D
// --------------------------------------------------------------------------------------
open Internal
open Setup

type Point3D = 
  { X : float; Y : float; Z : float }

type Material = 
  { GetMaterial : unit -> THREE.Material
    RequiresMovie : bool }

type Context =
  { Parent : THREE.Object3D
    Material : THREE.Material 
    Position : Point3D
    Rotation : Point3D 
    RequiresMovie : bool ref }

type Fun3D = 
  { Render : Context -> unit }

type FunColor = int

let ($) (scene1:Fun3D) (scene2:Fun3D) = 
  { Render = fun ctx ->
      scene1.Render(ctx)
      scene2.Render(ctx) }

module Mat = 
  /// Returns a material that paints objects with the picture from <br />
  /// the camera. You can pass the material to <code>Fun.material</code>.
  let movie = 
    { GetMaterial = fun () -> Globals.eval("document.fun3dMovieMaterial") |> unbox 
      RequiresMovie = true }

  /// Creates a material that can be passed to <code>Fun.material</code>. You can <br />
  /// choose one of the colors available in <code>Color</code> such as <code>Color.yellow</code>.
  let flat (color:FunColor) = 
    { GetMaterial = fun () -> THREE.MeshBasicMaterial.Create(unbox (colorObj color)) :> _ 
      RequiresMovie = false } 

module Color =
  /// <span style="display:inline-block;background:#ff0000;width:30px;margin:0px 5px 0px 0px">&#160;</span> 
  /// Red color <code>0xff0000</code> that can be passed to <code>Mat.flat</code>
  let red : FunColor = 0xff0000
  /// <span style="display:inline-block;background:#00ff00;width:30px;margin:0px 5px 0px 0px">&#160;</span> 
  /// Green color <code>0x00ff00</code> that can be passed to <code>Mat.flat</code>
  let green : FunColor = 0x00ff00
  /// <span style="display:inline-block;background:#0000ff;width:30px;margin:0px 5px 0px 0px">&#160;</span> 
  /// Blue color <code>0x0000ff</code> that can be passed to <code>Mat.flat</code>
  let blue : FunColor = 0x0000ff
  /// <span style="display:inline-block;background:#ffff00;width:30px;margin:0px 5px 0px 0px">&#160;</span> 
  /// Yellow color <code>0xffff00</code> that can be passed to <code>Mat.flat</code>
  let yellow : FunColor = 0xffff00
  /// <span style="display:inline-block;background:#ff00ff;width:30px;margin:0px 5px 0px 0px">&#160;</span> 
  /// Purple color <code>0xff00ff</code> that can be passed to <code>Mat.flat</code>
  let purple : FunColor = 0xff00ff
  /// <span style="display:inline-block;background:#00ffff;width:30px;margin:0px 5px 0px 0px">&#160;</span> 
  /// Cyan color <code>0x00ffff</code> that can be passed to <code>Mat.flat</code>
  let cyan : FunColor = 0x00ffff
  /// <span style="display:inline-block;background:#ffa500;width:30px;margin:0px 5px 0px 0px">&#160;</span> 
  /// Orange color <code>0xffa500</code> that can be passed to <code>Mat.flat</code>
  let orange : FunColor = 0xffa500

module Fun = 
  let private f3d f = { Render = f }

  /// Creates a cube object in the centre of the world
  let cube = f3d <| fun ctx ->
    let geometry = THREE.BoxGeometry.Create( 1., 1., 1. )
    let cube = THREE.Mesh.Create( geometry, ctx.Material )
    ctx.Parent.add(cube) 

  /// Creates a cylinder object in the centre of the world
  let cylinder = f3d <| fun ctx ->
    let geometry = THREE.CylinderGeometry.Create( 0.5, 0.5, 1., 32., 1. )
    let cylinder = THREE.Mesh.Create( geometry, ctx.Material )
    ctx.Parent.add( cylinder )

  /// Creates a cone object in the centre of the world
  let cone = f3d <| fun ctx ->
    let geometry = THREE.CylinderGeometry.Create( 0., 0.5, 1., 32., 1. )
    let cone = THREE.Mesh.Create( geometry, ctx.Material )
    ctx.Parent.add( cone )

  /// Creates a sphere object in the centre of the world
  let sphere = f3d <| fun ctx ->
    let geometry = THREE.SphereGeometry.Create( 1., 256., 256. )
    let cone = THREE.Mesh.Create( geometry, ctx.Material )
    ctx.Parent.add( cone )

  /// Creates a text object in the centre of the world. Try for <br />
  /// example <code>Fun.text "Hello!"</code> to create a message
  let text message = f3d <| fun ctx ->
    let geometry = THREE.TextGeometry.Create(message)
    let cone = THREE.Mesh.Create( geometry, ctx.Material )
    ctx.Parent.add( cone )

  /// Changes the color of all objects wrapped in this call to the <br />
  /// specified color. You can find pre-defined colors in <code>Color</code>.<br />
  /// Try for example <code>Fun.cube |&gt; Fun.color Color.orange</code>
  let color color (scene:Fun3D) = f3d  <| fun ctx -> 
    scene.Render { ctx with Material = THREE.MeshPhongMaterial.Create(unbox (fancyMaterial color)) } 

  /// Changes the material of all objects wrapped in this call to the <br />
  /// specified material. You can find pre-defined materials in <code>Mat</code>.<br />
  /// Try for example <code>Fun.cube |&gt; Fun.material Mat.video</code>
  let material (material:Material) (scene:Fun3D) = f3d  <| fun ctx -> 
    ctx.RequiresMovie := material.RequiresMovie
    scene.Render { ctx with Material = material.GetMaterial() } 

  /// Make the wrapped objects smaller or bigger. This takes three <br />
  /// numbers to specify the scaling of depth, height and width. <br />
  /// Try for example <code>Fun.cube |&gt; Fun.scale (5.0, 5.0, 0.5)</code>
  let scale (sx,sy,sz) (scene:Fun3D) = f3d <| fun ctx ->
    let root = THREE.Object3D.Create()
    root.scale.set(sx,sy,sz) |> ignore
    scene.Render { ctx with Parent = root }
    ctx.Parent.add(root)

  /// Move the object from the center of the world. This takes <br />
  /// three numbers to specify the offsets. Try for example:<br />
  /// <code>Fun.cube $ (Fun.cube |&gt; Fun.move (-2.0, 0.0, 0.0))</code>
  let move (dx,dy,dz) (scene:Fun3D) = f3d <| fun ctx ->
    let root = THREE.Object3D.Create()
    root.translateX(dx)
    root.translateY(dy)
    root.translateZ(dz)
    scene.Render { ctx with Parent = root }
    ctx.Parent.add(root)

  /// Rotate the object using three specified angles in degrees. <br />
  /// Try for example <code>Fun.cone |&gt; Fun.rotate (45.0, 0.0, 0.0)</code>
  let rotate (rx,ry,rz) (scene:Fun3D) = f3d <| fun ctx ->
    let root = THREE.Object3D.Create()
    root.rotation.set
      ( rx/180.0*System.Math.PI,
        ry/180.0*System.Math.PI,
        rz/180.0*System.Math.PI) |> ignore
    scene.Render { ctx with Parent = root }
    ctx.Parent.add(root)

  /// You should not need to call this function. <br />
  /// All shapes are created automatically!
  let show (it:Fun3D) =
    // Call `setupScene` just once
    if isNull(Globals.eval("document.fun3dCamera")) then setupScene()
    let camera = Globals.eval("document.fun3dCamera") |> unbox<THREE.PerspectiveCamera>
    let root = Globals.eval("document.fun3dRoot") |> unbox<THREE.Object3D>
    
    let requiresMovie = ref false
    let ctx = 
      { Parent = root
        RequiresMovie = requiresMovie
        Material = THREE.MeshPhongMaterial.Create(unbox (fancyMaterial 0xffffff))
        Position = { X = 0.; Y = 0.; Z = 0. }
        Rotation = { X = 0.; Y = 0.; Z = 0. } }

    while root.children.length <> 0. do
      root.remove(root.children.[0])

    it.Render(ctx)
    
    if requiresMovie.Value then 
      if isNull(Globals.eval("document.fun3dMovieMaterial")) then 
        setupVideo (fun _ -> it.Render(ctx))
