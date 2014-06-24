module ElmGL where

import Math.Vector3(..)
import Math.Matrix4(..)
import Graphics.WebGL(..)
import Window

{--- Types ---}

type Vertex = { pos:Vec3, col:Vec3, norm:Vec3 }

{--- Shaders ---}

vertexShader : Shader
                { attr | pos:Vec3, col:Vec3, norm:Vec3}
                { unif | projection:Mat4, modelView:Mat4 }
                { vcol:Vec3, vnorm:Vec3 }

vertexShader = [glsl|
    precision mediump float;
    attribute vec3 pos;
    attribute vec3 col;
    attribute vec3 norm;
    uniform mat4 projection;
    uniform mat4 modelView;
    varying vec3 vcol;
    varying vec3 vnorm;

    void main() {
        gl_Position = projection * modelView * vec4(pos, 1.0);
        vcol = col;
        vnorm = norm;
    }
|]

fragmentShader : Shader
                    { }
                    { unif | projection:Mat4, modelView:Mat4 }
                    { vcol:Vec3, vnorm:Vec3 }

fragmentShader = [glsl|
    precision mediump float;
    varying vec3 vcol;
    varying vec3 vnorm;

    void main() {
        gl_FragColor = vec4(vcol, 1.0);
    }
|]

{--- Meshes' definitions ---}

triangleMesh : [Triangle Vertex]
triangleMesh = [
        (Vertex (vec3 -1.0 -1.0  0.0) (vec3 1.0 0.0 0.0) (vec3 0.0 0.0 -1.0),
         Vertex (vec3  1.0 -1.0  0.0) (vec3 1.0 0.0 0.0) (vec3 0.0 0.0 -1.0),
         Vertex (vec3  0.0  1.5  0.0) (vec3 1.0 1.0 0.0) (vec3 0.0 0.0 -1.0))]

triangle : Mat4 -> Mat4 -> Entity
triangle projection modelView = entity vertexShader fragmentShader triangleMesh
                                { projection = projection, modelView = modelView }

{--- ModelView matrices ---}

rotatingModelView : Float -> Mat4
rotatingModelView time = makeRotate (time * 0.001) (vec3 0.0 0.0 1.0)


{--- Projection matrix ---}

projectionScene : (Int, Int) -> Mat4
projectionScene (width, height) =
    let
        fovy = 45.0
        aspect = (toFloat width) / (toFloat height)
        znear = 0.01
        zfar = 100.0
        eye = vec3 0.0 0.0 -5.0
        center = vec3 0.0 0.0 0.0
        up = vec3 0.0 1.0 0.0
    in
        (makePerspective fovy aspect znear zfar) `mul` (makeLookAt eye center up)

{--- GL Scene ---}

scene : (Int, Int) -> Float -> Element
scene (width, height) time =
    let
        projection = projectionScene (width, height)
        modelView = rotatingModelView time
    in
        webgl (width, height) [triangle projection modelView]

{--- Page layout ---}

header = [markdown|
A Web GLSL Demo.
|]

footer = [markdown|
&copy; 2014 Marcin Osowski
|]

content : (Int, Int) -> Float -> Element
content (width, height) time =
    container width height midTop <|
    flow down [
        spacer 1 25,
        header, 
        color darkCharcoal <| scene (450, 300) time,
        footer
    ]


{--- Main ---}

time : Signal Float
time = foldp (+) 0.0 (fps 5)

main = content <~ Window.dimensions ~ time

