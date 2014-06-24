module ElmGL where

import Math.Vector3(..)
import Math.Matrix4(..)
import Graphics.WebGL(..)
import Math.Vector3
import Window
import Mouse

{--- Types ---}

type Vertex = { pos:Vec3, col:Vec3, norm:Vec3 }

{--- Shaders ---}

vertexShader : Shader
                { attr | pos:Vec3, col:Vec3, norm:Vec3}
                { unif | proj:Mat4, modelView:Mat4, lightSrc:Vec3}
                { vcol:Vec3 }

vertexShader = [glsl|
    precision mediump float;
    attribute vec3 pos;
    attribute vec3 col;
    attribute vec3 norm;
    uniform mat4 proj;
    uniform mat4 modelView;
    uniform vec3 lightSrc;
    varying vec3 vcol;

    void main() {
        vec4 afterModelView = modelView * vec4(pos, 1.0);
        vec3 toLightSrc = lightSrc - afterModelView.xyz;
        float intensity = dot(normalize(toLightSrc), normalize(norm));
        if(intensity < 0.2) {
            intensity = 0.2;
        }
        
        gl_Position = proj * afterModelView;
        vcol = col * intensity;
    }
|]

fragmentShader : Shader
                    { }
                    { unif | proj:Mat4, modelView:Mat4, lightSrc:Vec3 }
                    { vcol:Vec3 }

fragmentShader = [glsl|
    precision mediump float;
    varying vec3 vcol;

    void main() {
        gl_FragColor = vec4(vcol, 1.0);
    }
|]

{--- Meshes' definitions ---}

triangleMesh : [Triangle Vertex]
triangleMesh = [
        (Vertex (vec3 -1.0 -1.0  0.0) (vec3 1.0 0.0 0.0) (vec3 -1.0 -1.0 -5.0),
         Vertex (vec3  1.0 -1.0  0.0) (vec3 1.0 0.0 0.0) (vec3  1.0 -1.0 -5.0),
         Vertex (vec3  0.0  1.5  0.0) (vec3 1.0 1.0 0.0) (vec3  0.0  1.5 -5.0))]


sphereMesh : Int -> Int -> Vec3 -> [Triangle Vertex]
sphereMesh latSplits lonSplits col =
    let
        latLayersFun = sphereMeshLat latSplits lonSplits col
        latLayers = [0..(latSplits - 1)]
    in
        concat <| map latLayersFun latLayers

sphereMeshLat : Int -> Int -> Vec3 -> Int -> [Triangle Vertex]
sphereMeshLat latSplits lonSplits col latLayer =
    let
        lonLayersFun = sphereMeshLatLon latSplits lonSplits col latLayer
        lonLayers = [0..(lonSplits - 1)]
    in
        concat <| map lonLayersFun lonLayers

sphereMeshLatLon : Int -> Int -> Vec3 -> Int -> Int -> [Triangle Vertex]
sphereMeshLatLon latSplits lonSplits col latLayer lonLayer =
    let
        latA = (toFloat (latLayer + 0) / toFloat latSplits - 0.5) * pi
        latB = (toFloat (latLayer + 1) / toFloat latSplits - 0.5) * pi
        lonA = (toFloat (lonLayer + 0) / toFloat lonSplits - 0.5) * 2.0 * pi
        lonB = (toFloat (lonLayer + 1) / toFloat lonSplits - 0.5) * 2.0 * pi
        point00 = sphereMeshPoint latA lonA
        point01 = sphereMeshPoint latA lonB
        point10 = sphereMeshPoint latB lonA
        point11 = sphereMeshPoint latB lonB
    in
        [(Vertex point00 col point00,
          Vertex point01 col point01,
          Vertex point10 col point10),
         (Vertex point01 col point01,
          Vertex point11 col point11,
          Vertex point10 col point10)]


sphereMeshPoint : Float -> Float -> Vec3
sphereMeshPoint lat lon =
    let
        x = (cos lat) * (cos lon)
        y = sin lat
        z = (cos lat) * (sin lon)
    in vec3 x y z


cubeMesh : Vec3 -> Float -> [Triangle Vertex]
cubeMesh col edgeSize =
    let
        point000 = vec3 -1.0 -1.0 -1.0
        point001 = vec3 -1.0 -1.0  1.0
        point010 = vec3 -1.0  1.0 -1.0
        point011 = vec3 -1.0  1.0  1.0
        point100 = vec3  1.0 -1.0 -1.0
        point101 = vec3  1.0 -1.0  1.0
        point110 = vec3  1.0  1.0 -1.0
        point111 = vec3  1.0  1.0  1.0
        dir001 = vec3 0.0 0.0 1.0
        dir010 = vec3 0.0 1.0 0.0
        dir100 = vec3 1.0 0.0 0.0
    in
        concat <| 
            [cubeEdge col edgeSize point000 point001 dir010,
             cubeEdge col edgeSize point000 point001 dir100,
             cubeEdge col edgeSize point001 point011 dir100,
             cubeEdge col edgeSize point001 point011 (negate dir001),
             cubeEdge col edgeSize point011 point010 (negate dir010),
             cubeEdge col edgeSize point011 point010 dir100,
             cubeEdge col edgeSize point010 point000 dir001,
             cubeEdge col edgeSize point010 point000 dir100,

             cubeEdge col edgeSize point000 point100 dir001,
             cubeEdge col edgeSize point000 point100 dir010,
             cubeEdge col edgeSize point010 point110 dir001,
             cubeEdge col edgeSize point010 point110 (negate dir010),
             cubeEdge col edgeSize point011 point111 (negate dir010),
             cubeEdge col edgeSize point011 point111 (negate dir001),
             cubeEdge col edgeSize point001 point101 dir010,
             cubeEdge col edgeSize point001 point101 (negate dir001),

             cubeEdge col edgeSize point100 point101 dir010,
             cubeEdge col edgeSize point100 point101 (negate dir100),
             cubeEdge col edgeSize point101 point111 (negate dir100),
             cubeEdge col edgeSize point101 point111 (negate dir001),
             cubeEdge col edgeSize point111 point110 (negate dir010),
             cubeEdge col edgeSize point111 point110 (negate dir100),
             cubeEdge col edgeSize point110 point100 dir001,
             cubeEdge col edgeSize point110 point100 (negate dir100)
             ]


cubeEdge : Vec3 -> Float -> Vec3 -> Vec3 -> Vec3 -> [Triangle Vertex]
cubeEdge col edgeSize pointA pointB dir =
    let
        delta = Math.Vector3.scale edgeSize dir
        pointA0 = pointA
        pointA1 = add pointA delta
        pointB0 = pointB
        pointB1 = add pointB delta
        normal = add (add pointA pointB) (Math.Vector3.scale 2.0 dir)
    in
        [(Vertex pointA0 col normal,
          Vertex pointA1 col normal,
          Vertex pointB1 col normal),
         (Vertex pointA0 col normal,
          Vertex pointB0 col normal,
          Vertex pointB1 col normal)]



         

{--- ModelView matrix ---}

translateModelView : Vec3 -> Mat4
translateModelView = makeTranslate

{--- Projection matrix ---}

projScene : (Int, Int) -> (Int, Int) -> Mat4
projScene (width, height) (xRot, yRot) =
    let
        fovy = 45.0
        aspect = (toFloat width) / (toFloat height)
        znear = 0.01
        zfar = 100.0
        eye = vec3 0.0 0.0 -7.0
        center = vec3 0.0 0.0 0.0
        up = vec3 0.0 1.0 0.0
        yRotAngle = toFloat yRot / 100.0
        yRotVec = vec3 -1.0 0.0 0.0
        xRotAngle = toFloat xRot / 100.0
        xRotVec = vec3 0.0 1.0 0.0
    in
        (makePerspective fovy aspect znear zfar)
            `mul` (makeLookAt eye center up)
            `mul` (makeRotate yRotAngle yRotVec)
            `mul` (makeRotate xRotAngle xRotVec)

{--- GL Scene ---}

scene : (Int, Int) -> (Int, Int) -> Element
scene (width, height) (xRot, yRot) =
    let
        proj = projScene (width, height) (xRot, yRot)
        modelView = translateModelView (vec3 0.0 0.0 0.0) 
        lightSrc = vec3 5.0 5.0 -6.0
        shadedEntity = entity vertexShader fragmentShader
        uniforms = { proj=proj, modelView=modelView, lightSrc=lightSrc }

        triangle = shadedEntity triangleMesh uniforms

        sphereCol = vec3 0.0 0.0 1.0
        sphere = shadedEntity (sphereMesh 6 10 sphereCol) uniforms

        cubeCol = vec3 1.0 1.0 0.0
        cubeEdgeSize = 0.15
        cube = shadedEntity (cubeMesh cubeCol cubeEdgeSize) uniforms
    in
        webgl (width, height) [{-triangle-} sphere, cube]

{--- Page layout ---}

header = [markdown|
A Web GLSL Demo.
|]

footer = [markdown|
&copy; 2014 Marcin Osowski
|]

content : (Int, Int) -> (Int, Int) -> Element
content (width, height) (xRot, yRot) =
    container width height midTop <|
    flow down [
        spacer 1 25,
        header, 
        color darkCharcoal <| scene (400, 250) (xRot, yRot),
        footer
    ]


{--- Main ---}

mouseRot : Signal (Int, Int)
mouseRot =
    let
        mouseDownPosition = lift3 (,,) Mouse.isDown Mouse.x Mouse.y
        mouseDeltaFun (isDown, x2, y2) (x1, y1, xRot, yRot) =
            if isDown then
                (x2, y2, xRot + x2 - x1, yRot + y2 - y1)
            else
                (x2, y2, xRot, yRot)

        projection (x, y, xRot, yRot) = (xRot, yRot)

    in
        projection <~ foldp mouseDeltaFun (0, 0, 0, 0) mouseDownPosition


main = content <~ Window.dimensions ~ mouseRot

