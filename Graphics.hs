module Graphics where
import Graphics.UI.GLUT 
import Graphics.Rendering.OpenGL 

import Game

{-
draw :: GameState -> IO ()
draw gs = do
    clear [ColorBuffer]
    matrixMode $= Modelview 0
    color (Color3 1.0 1.0 1.0)
    renderPrimitive TriangleStrip quad
    flush
-}   

vertex2f :: GLfloat -> GLfloat -> IO ()
vertex2f a b = vertex $ Vertex2 a b

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g b

quad :: IO ()
quad = do
    vertex2f (-1) (-1)
    vertex2f (-1) 1
    vertex2f 1 (-1)
    vertex2f 1 1
    vertex2f (-1) 1
    
