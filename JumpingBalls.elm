module JumpingBalls where

import Math.Vector3(..)
import Math.Matrix4(..)
import Graphics.WebGL(..)
import Math.Vector3
import Graphics.Input
import Window
import Mouse
import Random

{--- Types ---}

type Vertex = { pos:Vec3, col:Vec3, norm:Vec3 }
type Sphere = { pos:Vec3, vel:Vec3, col:Vec3, radius:Float }
type Cube = { col:Vec3, edgeWidth:Float }

type GLSLattr = { pos:Vec3, col:Vec3, norm:Vec3 }
type GLSLunif = { proj:Mat4, modelView:Mat4, lightSrc:Vec3, symmLight:Float }
type GLSLvary = { vcol:Vec3 }

data Keys = AddNew | RemoveAll | Empty

{--- Shaders ---}

vertexShader : Shader GLSLattr GLSLunif GLSLvary
vertexShader = [glsl|
    precision mediump float;
    attribute vec3 pos;
    attribute vec3 col;
    attribute vec3 norm;
    uniform mat4 proj;
    uniform mat4 modelView;
    uniform vec3 lightSrc;
    uniform float symmLight;
    varying vec3 vcol;

    void main() {
        vec4 afterModelView = modelView * vec4(pos, 1.0);
        vec3 toLightSrc = lightSrc - afterModelView.xyz;
        float intensity = 1.25 * dot(normalize(toLightSrc), normalize(norm));
        if(intensity < 0.0) {
            intensity = -symmLight * intensity;
        }
        if(intensity < 0.2) {
            intensity = 0.2;
        }
        if(intensity > 1.0) {
            intensity = 1.0;
        }

        gl_Position = proj * afterModelView;
        vcol = col * intensity;
    }
|]

fragmentShader : Shader {} GLSLunif GLSLvary
fragmentShader = [glsl|
    precision mediump float;
    varying vec3 vcol;

    void main() {
        gl_FragColor = vec4(vcol, 1.0);
    }
|]

{--- Meshes' definitions ---}

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

{--- Entities' definitions ---}

sphereEntity : GLSLunif -> Sphere -> Entity
sphereEntity unifs s =
    let
        newModelView = scale3 s.radius s.radius s.radius <| translate s.pos <| unifs.modelView
        newUnifs = { proj=unifs.proj, modelView=newModelView,
                     lightSrc=unifs.lightSrc, symmLight=0.0 }
        mesh = sphereMesh 10 14 s.col
    in
        entity vertexShader fragmentShader mesh newUnifs

cubeEntity : GLSLunif -> Cube -> Entity
cubeEntity unifs c =
    let
        mesh = cubeMesh c.col c.edgeWidth
        newUnifs = { proj=unifs.proj, modelView=unifs.modelView,
                     lightSrc=unifs.lightSrc, symmLight=0.8 }
    in
        entity vertexShader fragmentShader mesh newUnifs

{--- Projection matrix ---}

projMatrix : (Int, Int) -> (Int, Int) -> Mat4
projMatrix (width, height) (xRot, yRot) =
    let
        fovy = 45.0
        aspect = (toFloat width) / (toFloat height)
        znear = 0.01
        zfar = 100.0
        eye = vec3 0.0 0.0 -4.0
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

scene : (Int, Int) -> (Int, Int) -> [Sphere]-> Element
scene (width, height) (xRot, yRot) spheres =
    let
        proj = projMatrix (width, height) (xRot, yRot)
        modelView = identity
        lightSrc = vec3 4.0 5.0 -6.0
        unifs = { proj=proj, modelView=modelView, lightSrc=lightSrc, symmLight=0.0 }

        cubeCol = vec3 1.0 1.0 0.0
        cubeEdgeWidth = 0.15
        cube = { col=cubeCol, edgeWidth=cubeEdgeWidth}
        entities = [cubeEntity unifs cube] ++ (map (sphereEntity unifs) spheres)
    in
        webgl (width, height) entities

{--- Physics - spheres movement logic ---}

iterSpheres : (Float, Keys, Vec3, Int) -> [Sphere] -> [Sphere]
iterSpheres (dt, k, randVel, col) spheres =
    if k == RemoveAll then
        []
    else if k == AddNew then
        spheres ++ [newSphere randVel col]
    else
        let
            newSpheres = map (iterSphere dt spheres) spheres
        in
            map (bounceSphereCube . bounceSphereSpheres newSpheres) newSpheres


iterSphere : Float -> [Sphere] -> Sphere -> Sphere
iterSphere dt spheres s =
    let
        pos = add s.pos <| Math.Vector3.scale (dt/1000.0) s.vel
        sphere = { pos=pos, vel=s.vel, col=s.col, radius=s.radius }
    in
        sphere

bounceSphereCube : Sphere -> Sphere
bounceSphereCube s =
    let
        r = s.radius
        col = s.col

        (x1, vx1) = bounceAxisCube (getX s.pos) (getX s.vel) r
        (y1, vy1) = bounceAxisCube (getY s.pos) (getY s.vel) r
        (z1, vz1) = bounceAxisCube (getZ s.pos) (getZ s.vel) r

        pos = vec3 x1 y1 z1
        vel = vec3 vx1 vy1 vz1
    in
        { pos=pos, vel=vel, col=col, radius=r }

bounceAxisCube : Float -> Float -> Float -> (Float, Float)
bounceAxisCube x0 v0 r =
    let
        (x1, v1) =
            if x0 + r > 1.0 then
                (2.0 - x0 - 2.0*r, -v0)
            else
                (x0, v0)
    in
            if x1 - r < -1.0 then
                (2.0*r - 2.0 - x1, -v1)
            else
                (x1, v1)

bounceSphereSpheres : [Sphere] -> Sphere -> Sphere
bounceSphereSpheres spheres sphere = foldl bounceSphereSphere sphere spheres

bounceSphereSphere : Sphere -> Sphere -> Sphere
bounceSphereSphere other this =
    let
        dir = Math.Vector3.sub other.pos this.pos
        dist = Math.Vector3.length dir
        radiusSum = this.radius + other.radius
    in
        if dist < 1e-7 then
            this
        else if dist > radiusSum then
            this
        else
            let
                normDir = Math.Vector3.scale (1.0/dist) dir
                deltaPos = Math.Vector3.scale ((radiusSum - dist) * -0.5) dir
                deltaVel1Val = -(Math.Vector3.dot this.vel normDir)
                deltaVel2Val = Math.Vector3.dot other.vel normDir
                deltaVel1 = Math.Vector3.scale deltaVel1Val normDir
                deltaVel2 = Math.Vector3.scale deltaVel2Val normDir
                deltaVel = Math.Vector3.add deltaVel1 deltaVel2

                newPos = Math.Vector3.add this.pos deltaPos
                newVel = Math.Vector3.add this.vel deltaVel
            in
                { pos=newPos, vel=newVel, col=this.col, radius=this.radius }


newSphere : Vec3 -> Int -> Sphere
newSphere vel colIdx =
    let
        col =
            if colIdx == 0 then
                vec3 0.973 0.075 0.000
            else if colIdx == 1 then
                vec3 0.941 0.486 0.020
            else if colIdx == 2 then
                vec3 0.894 0.722 0.000
            else if colIdx == 3 then
                vec3 0.071 0.545 0.039
            else
                vec3 0.008 0.494 0.541
        pos = vec3 0.0 0.0 0.0
        radius = 0.30
    in
        { pos=pos, vel=vel, col=col, radius=radius }

initialSphere : Sphere
initialSphere = newSphere (vec3 1.0 0.7 1.3) 4

mergeTimeKeys : Signal Float -> Signal Keys -> Signal (Float, Keys, Vec3, Int)
mergeTimeKeys time keys =
    let
        counter = count keys
        buildVel x y z = Math.Vector3.sub (vec3 0.5 0.5 0.5)
                            <| Math.Vector3.scale 3.5
                            <| vec3 x y z
        vel = buildVel <~ Random.float keys ~ Random.float keys ~ Random.float keys
        col = Random.range 0 4 keys
        zipped = (,,,,) <~ time ~ keys ~ counter ~ vel  ~ col
        folder (time1, keys1, counter1, vel1, col1) (time0, keys0, counter0, vel0, col0) = 
            if counter0 == counter1 then
                (time1, Empty, counter1, vel1, col1)
            else
                (time1, keys1, counter1, vel1, col1)
        projection (time, keys, counter, vel, col) = (time, keys, vel, col)
        initState = (0, Empty, 0, (vec3 0.0 0.0 0.0), 0)
    in
        projection <~ foldp folder initState zipped
        

spheres : Signal Float -> Signal Keys -> Signal [Sphere]
spheres time keys = foldp iterSpheres [initialSphere] <| mergeTimeKeys time keys

{--- Static content ---}

header = [markdown|
A Web GL Demo. Drag to rotate.
|]

footer = [markdown|
&copy; 2014 Marcin Osowski
|]

{--- Buttons handler ---}

buttonKeys : Graphics.Input.Input Keys
buttonKeys = Graphics.Input.input Empty

{--- Page layout ---}

layout : (Int, Int) -> (Int, Int) -> [Sphere] -> Element
layout (width, height) (xRot, yRot) spheres =
    container width height midTop <|
    flow down [
        spacer 1 50,
        header, 
        color darkCharcoal <| scene (600, 375) (xRot, yRot) spheres,
        spacer 1 7,
        flow right [
            Graphics.Input.button buttonKeys.handle AddNew "Add new",
            Graphics.Input.button buttonKeys.handle RemoveAll "Remove all"
        ],
        footer
    ]


{--- Main ---}

mouseRot : Signal (Int, Int)
mouseRot =
    let
        mouseDownPosition = (,,) <~ Mouse.isDown ~ Mouse.x ~ Mouse.y
        mouseDeltaFun (isDown, x2, y2) (x1, y1, xRot, yRot) =
            if isDown then
                (x2, y2, xRot + x2 - x1, yRot + y2 - y1)
            else
                (x2, y2, xRot, yRot)

        projection (x, y, xRot, yRot) = (xRot, yRot)

    in
        projection <~ foldp mouseDeltaFun (0, 0, 35, 25) mouseDownPosition


main = layout <~ Window.dimensions ~ mouseRot ~ spheres (fps 30) buttonKeys.signal

