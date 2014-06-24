module ElmGL where

import Math.Vector3(..)
import Math.Vector4(..)
import Math.Matrix4(..)
import Graphics.WebGL(..)
import Window

{--- GLSL stuff ---}

type Vertex = { position:Vec4, color:Vec4 }

triangleVertex : Shader { attr | position:Vec4, color:Vec4 } { unif | view:Mat4 } { vcolor:Vec4 }
triangleVertex = [glsl|

precision mediump float;
attribute vec4 position;
attribute vec4 color;
uniform mat4 view;
varying vec4 vcolor;

void main() {
    gl_Position = view * position;
    vcolor = color;
}

|]


triangleFragment : Shader {} { unif | view:Mat4 } { vcolor:Vec4 }
triangleFragment = [glsl|

precision mediump float;
uniform mat4 view;
varying vec4 vcolor;

void main() {
    gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0); //vcolor;
}

|]

triangleMesh : [Triangle Vertex]
triangleMesh = [
        (Vertex (vec4 -1.0 -1.0  0.0  1.0) (vec4 1.0 0.0 0.0 1.0),
         Vertex (vec4  1.0 -1.0  0.0  1.0) (vec4 1.0 0.0 0.0 1.0),
         Vertex (vec4  0.0  1.5  0.0  1.0) (vec4 1.0 0.0 0.0 1.0))]

triangle : Mat4 -> Entity
triangle view = entity triangleVertex triangleFragment triangleMesh { view = view }

sceneView : (Int, Int) -> Float -> Mat4
sceneView (width, height) t =
    let
        perspectiveAngle = 45.0
        proportions = (toFloat width) / (toFloat height)
        minDepth = 0.01
        maxDepth = 100.0
        eye = vec3 0.0 0.0 -5.0
        center = vec3 0.0 0.0 0.0
        up = vec3 (sin (t/1000.0)) (cos (t/1000.0)) 0.0
    in
        mul (makePerspective perspectiveAngle proportions minDepth maxDepth)
            (makeLookAt eye center up)

scene : (Int, Int) -> Float -> Element
scene (width, height) t =
    let
        view = sceneView (width, height) t
    in
        webgl (width, height) [triangle view]

{--- Page layout ---}

header = [markdown|
A Web GLSL Demo.
|]

footer = [markdown|
&copy; 2014 Marcin Osowski
|]

content : (Int, Int) -> Float -> Element
content (width, height) t =
    container width height midTop <|
    flow down [
        spacer 1 25,
        header, 
        color darkCharcoal <| scene (450, 300) t,
        footer
    ]


{--- Main ---}

time : Signal Float
time = foldp (+) 0.0 (fps 15)

main = content <~ Window.dimensions ~ time

