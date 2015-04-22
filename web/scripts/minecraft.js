var SIZE = 50;
 
var materials = {};
var scene;
var videoTexture;
var renderer;
var video;
var camera;
var root;

function de2ra(degree) {
  return degree * (Math.PI/180);
}
// this function is executed on each animation frame
function animate()
{
  renderer.setClearColor( 0xaaaaff, 1 );
  // render
  renderVideo();
  renderer.render(scene, camera);
 
  // request new frame
  requestAnimationFrame(animate);
}
 
function initBasics() {
  // renderer
  renderer = new THREE.WebGLRenderer();
  var first = document.getElementById("first");
  renderer.setSize(first.offsetWidth, first.offsetHeight);
  first.appendChild(renderer.domElement);
 
  // camera
  camera = new THREE.PerspectiveCamera(45, first.offsetWidth / first.offsetHeight, 1, 1000);
  camera.position.z = 500;
 
  document.fun3dCamera = camera;

  // scene
  scene = new THREE.Scene();
  scene.rotation.x = de2ra(110);

  root = new THREE.Object3D();
  scene.add(root);

  document.addEventListener("keydown", function(e) {
    var code = e.keyCode;
    if (code == 189) {
      camera.position.z = camera.position.z + 10*3;
      camera.position.y = camera.position.y + 10*1;
    } else if (code == 187) {
      camera.position.z = camera.position.z - 10*3;
      camera.position.y = camera.position.y - 10*1; 
    }
    if (e.getModifierState("Control") || e.getModifierState("Alt")) {
      if (code == 38) {
        camera.position.z = camera.position.z - 10 * 1;
        camera.position.y = camera.position.y - 10 * 3;
      } else if (code == 40) {
        camera.position.z = camera.position.z + 10 * 1;
        camera.position.y = camera.position.y + 10 * 3;
      } else if (code == 37) {
        camera.position.x = camera.position.x + 10 * 3;
      } else if (code == 39) {
        camera.position.x = camera.position.x - 10 * 3;
      }
    }
  });
}
 
function renderVideo() 
{ 
  if ( video.readyState === video.HAVE_ENOUGH_DATA ) 
  {
    //  alert('ok');
    videoImageContext.drawImage( video, 0, 0, videoImage.width, videoImage.height );
    if ( videoTexture ) {
      videoTexture.needsUpdate = true;
    }
  }
 
  renderer.render( scene, camera );
}
 
function initLight() {
  var ambientLight = new THREE.AmbientLight(0xbbbbbb);
  scene.add(ambientLight);
 
  // directional lighting
  var directionalLight = new THREE.DirectionalLight(0xffffff);
  directionalLight.position.set(1, 1, 1).normalize();
  scene.add(directionalLight);
}
 
function initFloor(wireframe) {
  var planeW = SIZE; // pixels
  var planeH = SIZE; // pixels 
  var numW = SIZE; // how many wide (50*50 = 2500 pixels wide)
  var numH = SIZE; // how many tall (50*50 = 2500 pixels tall)
  var plane = new THREE.Mesh(
      new THREE.PlaneGeometry( planeW*numW, planeH*numH, planeW, planeH ),
      materials.grass
  );
  plane.material.side = THREE.DoubleSide;
  scene.add(plane);
  if (!wireframe)
    return;
  var planeWireframe = new THREE.Mesh(
    new THREE.PlaneGeometry( planeW*numW, planeH*numH, planeW, planeH ),
    materials.wireframe
  );
  scene.add(planeWireframe);
}
 
var movieMaterial;
 
function initVideo() {
  video = document.getElementById( 'monitor' );
    
  videoImage = document.getElementById( 'videoImage' );
  videoImageContext = videoImage.getContext( '2d' );
  // background color if no video present
  videoImageContext.fillStyle = '#000000';
  videoImageContext.fillRect( 0, 0, videoImage.width, videoImage.height );
 
  videoTexture = new THREE.Texture( videoImage );
  videoTexture.minFilter = THREE.LinearFilter;
  videoTexture.magFilter = THREE.LinearFilter;
}
 
function addMaterialFromFile(name, filename) {
  materials[name] = new THREE.MeshLambertMaterial({
    map: THREE.ImageUtils.loadTexture(filename)
  });
}
 
function createMaterials() {
  materials.wireframe = new THREE.MeshBasicMaterial( {
    color: 0xffffff,
    wireframe: true
  });
  materials.video = new THREE.MeshLambertMaterial({
    map: videoTexture,
    overdraw: true,
    side: THREE.DoubleSide
  });
  addMaterialFromFile('grass', '/Content/grass.jpg');
  addMaterialFromFile('brick', '/Content/brick.jpg');
  addMaterialFromFile('water', '/Content/water.jpg');
  addMaterialFromFile('crate', '/Content/crate.jpg');
  addMaterialFromFile('stone', '/Content/stone.jpg');
  materials.grass.map.wrapS = THREE.RepeatWrapping;
  materials.grass.map.wrapT = THREE.RepeatWrapping;
  materials.grass.map.repeat.set( 50, 50 );
}

function init(useVideo) {
  if (scene == null) {
    initBasics();
    if (useVideo) initVideo();
    initLight();
    createMaterials();
    initFloor(true);
    // start animation
    animate();
  } else {
    while (root.children.length > 0)
      root.remove(root.children[0]);
  }
}

function block(x, y, z, kind) {
  var cube = new THREE.Mesh(new THREE.CubeGeometry(SIZE, SIZE, SIZE),
      materials[kind]);
  cube.overdraw = true;
  //cube.rotation.x = Math.PI * 0.1;
  cube.position.x = ((x || 0) - 2) * SIZE + SIZE / 2;
  cube.position.y = (y || 0) * SIZE + SIZE / 2;
  cube.position.z = ((z || 0) - 1) * SIZE + SIZE / 2;

  root.add(cube);
}