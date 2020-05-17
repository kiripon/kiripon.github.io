---
title: OpenGL in Haskell
date: 2014-12-04
---

haskellでOpenGLを使って何かしたい、と思って
[opengl-tutorial.org](http://www.opengl-tutorial.org/)を参考にhaddockを眺めながら書きました。haskellのOpenGLは、生のOpenGLと微妙に違うAPIを持っているので調べるのはかなりかったるかった。

ともあれ、赤い三角形はようやく表示できたので記念にソースコードを貼り付けておきます。

使用しているパッケージはGLFW-bではなくGLFWです。GLFW-bのほうはglfw3に対応しているようなので気が向いたら書きなおすことにしよう

_Main.hs_

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import           Control.Monad
import           Data.Array.Storable       (newListArray, withStorableArray)
import qualified Data.ByteString           as S8
import           Foreign.Ptr               (nullPtr)
import           Foreign.Storable          (sizeOf)
import           Graphics.Rendering.OpenGL (BufferObject, Color4 (..), GLfloat,
                                            VertexArrayObject, get, ($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import           System.Exit               (exitSuccess)

main :: IO ()
main = do
  putStrLn "initialize.."
  initialize
  putStrLn " done"
  print "begin loop"
  mainLoop
  print "end loop"
  GLFW.closeWindow
  GLFW.terminate
  where
    initialize = do
      GLFW.initialize >>= unless `flip` GLFW.terminate


      GLFW.openWindowHint GLFW.FSAASamples 4
      GLFW.openWindowHint GLFW.OpenGLVersionMajor 3
      GLFW.openWindowHint GLFW.OpenGLVersionMinor 2 -- 3はだめ？
      GLFW.openWindowHint GLFW.OpenGLForwardCompat True

      GLFW.openWindowHint GLFW.OpenGLProfile GLFW.OpenGLCoreProfile

      GLFW.windowTitle $= "GLFW Demo"
      wRes <- GLFW.openWindow (GL.Size 400 400)
              []
              GLFW.Window
      unless wRes $ do{
        putStrLn "failed to open window";
        GLFW.terminate;
        System.Exit.exitSuccess;}

      print "hgoe"
      vGL <- GL.get GL.glVersion
      putStrLn $ "OpenGL version:" ++ show vGL
      vGLFW <- GL.get GLFW.version
      putStrLn $ "GLFW version:" ++ show vGLFW


      GL.shadeModel            $= GL.Smooth
      GL.lineSmooth            $= GL.Enabled
      GL.blend                 $= GL.Enabled
      GL.blendFunc             $= (GL.SrcAlpha,GL.OneMinusSrcAlpha)
      GL.clearColor            $= Color4 0 0.5 0.5 0
      GLFW.windowSizeCallback  $= \size@(GL.Size w h) -> do{
          putStrLn "callback: windowResizeCallback";
          GL.viewport   $= (GL.Position 0 0,size);
          GL.matrixMode $= GL.Projection;
          GL.loadIdentity;
          GL.ortho2D 0 (realToFrac w) (realToFrac h) 0;
          }
      GLFW.windowCloseCallback $= do{
          putStrLn "callback: windowCloseCallback";
          GLFW.closeWindow;
          GLFW.terminate;
          exitSuccess;}

vertices :: [GLfloat]
vertices = [-1.0, -1.0, 0.0
           ,1.0, -1.0, 0.0
           ,0.0, 1.0, 0.0]


createVBO :: [GLfloat] -> IO BufferObject
createVBO elems = do
  [vertexBuffer] <- GL.genObjectNames 1
  GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
  arr <- newListArray (0,len-1)elems
  let bufSize = toEnum $ len * sizeOf (head elems)
  withStorableArray arr $ \ptr ->
    GL.bufferData GL.ArrayBuffer $= (bufSize,ptr,GL.StaticDraw)
  putStrLn "array buffered"
  GL.bindBuffer GL.ArrayBuffer $= Nothing -- なくてもいい？
  print "buffer binded"
  return vertexBuffer
  where
    len = length elems

mainLoop :: IO ()
mainLoop = do
  --GL.arrayPointer GL.
  --glGenVertexArrays(&vao)
  [vertexArrayId] <- GL.genObjectNames 1 :: IO [VertexArrayObject]
  --glBindVertexArray(vao)
  GL.bindVertexArrayObject $= Just vertexArrayId

  vbo <- createVBO vertices

  program <- loadShaders "./vertexShader.glsl" "./fragmentShader.glsl"
  GL.currentProgram $= Just program

  pState <- get $ GL.validateStatus program
  unless pState $ putStrLn "Shader validate Failed"

  loop vbo vertexArrayId
  return ()
  where
    loop vertexBuffer vao = do
      draw vertexBuffer vao
      GLFW.pollEvents
      GLFW.swapBuffers
      p <- GLFW.getKey GLFW.ESC
      unless (p == GLFW.Press) $ do
        GLFW.sleep 0.001
        loop vertexBuffer vao


draw :: BufferObject -> VertexArrayObject -> IO ()
draw vertexBuffer vao = do
  GL.clear [GL.ColorBuffer,GL.DepthBuffer]

  --glDrawArraysを書く
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  GL.bindBuffer GL.ArrayBuffer               $= Just vertexBuffer
  GL.bindVertexArrayObject $= Just vao
  GL.vertexAttribPointer (GL.AttribLocation 0)$= (GL.ToFloat,descriptor)
  GL.drawArrays GL.Triangles 0 3
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled

  GL.flush
  where
    descriptor= GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr
      :: GL.VertexArrayDescriptor GLfloat

loadShaders :: FilePath -> FilePath -> IO GL.Program
loadShaders vertexFilePath fragmentFilePath = do

  vertexShaderId   <- GL.createShader GL.VertexShader
  vShaderData <- S8.readFile vertexFilePath
  putStrLn $ "compilingShader : " ++ vertexFilePath
  GL.shaderSourceBS vertexShaderId $= vShaderData
  GL.compileShader vertexShaderId

  vShaderLog <- get $ GL.shaderInfoLog vertexShaderId
  putStrLn vShaderLog

  fragmentShaderId <- GL.createShader GL.FragmentShader
  fShaderData <- S8.readFile fragmentFilePath
  putStrLn $ "compilingShader : " ++ fragmentFilePath
  GL.shaderSourceBS fragmentShaderId $= fShaderData
  GL.compileShader fragmentShaderId
  fShaderLog <- get $ GL.shaderInfoLog fragmentShaderId
  putStrLn $ "shader log :\n" ++ fShaderLog

  --link Program
  putStrLn "Linking Shader Program"
  programId <- GL.createProgram
  GL.attachShader prattachShader programId vertexShaderId
  GL.ogramId vertexShaderId
  GL.attachShader programId fragmentShaderId
  GL.linkProgram programId

  --check program
  programLog <- get $ GL.programInfoLog programId
  putStrLn $ "program link log :\n" ++ programLog

  GL.deleteObjectName vertexShaderId
  GL.deleteObjectName fragmentShaderId
  return programId
```

_vertexShader.glsl_

```glsl
#version 330 core

layout(location = 0) in vec3 vertexPosition_modelspace;

void main(){
  gl_Position.xyz = vertexPosition_modelspace;
  gl_Position.w   = 1.0;
}
```

_fragmentShader.glsl_

```glsl
#version 330 core
out vec3 color;

void main(){
  color = vec3(1,0,0);
}
```
