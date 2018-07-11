module Fractal3
  (main)
  where
import           Apollony.Apollony3
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT

white,black,red :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
red        = Color4    1    0    0    1

data Context = Context
    {
      contextRot1    :: IORef GLfloat
    , contextRot2    :: IORef GLfloat
    , contextRot3    :: IORef GLfloat
    , contextSpheres :: IORef [((Double,Double),Double)]
    }

display :: Context -> IORef Double -> DisplayCallback
display context zoom = do
  clear [ColorBuffer, DepthBuffer]
  spheres <- get (contextSpheres context)
  zoom' <- get zoom
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  loadIdentity
  (_, size) <- get viewport
  resize zoom' size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  mapM_ (\(center,radius) -> preservingMatrix $ do
                  translate (toVector3 center)
                  materialDiffuse Front $= red
                  renderObject Solid $ Sphere' radius 64 64)
        spheres
  swapBuffers
  where
    toVector3 (x,y) = Vector3 x y 0

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-5.5+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -- zoom
         -> IORef Int -- depth
         -> IORef Double -- phi
         -> IORef Double -- beta
         -> IORef [((Double,Double),Double)]
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom depth phi beta spheres c _ = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+2)
    'm' -> zoom $~! (+0.1)
    'l' -> zoom $~! subtract 0.1
    'h' -> do
      depth $~! (+1)
      depth' <- get depth
      phi' <- get phi
      beta' <- get beta
      writeIORef spheres (fractal depth' phi' beta')
    'n' -> do
      depth $~! (\n -> if n>1 then n-1 else n)
      depth' <- get depth
      phi' <- get phi
      beta' <- get beta
      writeIORef spheres (fractal depth' phi' beta')
    'g' -> do
      depth' <- get depth
      phi $~! (\x -> if x<0.9 then x+0.1 else x)
      phi' <- get phi
      beta' <- get beta
      writeIORef spheres (fractal depth' phi' beta')
    'b' -> do
      depth' <- get depth
      phi $~! (\x -> if x>(-0.1) then x-0.1 else x)
      phi' <- get phi
      beta' <- get beta
      writeIORef spheres (fractal depth' phi' beta')
    'f' -> do
      depth' <- get depth
      phi' <- get phi
      beta $~! (+ pi/90)
      beta' <- get beta
      writeIORef spheres (fractal depth' phi' beta')
    'v' -> do
      depth' <- get depth
      phi' <- get phi
      beta $~! subtract (pi/90)
      beta' <- get beta
      writeIORef spheres (fractal depth' phi' beta')
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Apollony fractal"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= black
  materialAmbient FrontAndBack $= black
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-500) 1
  ambient (Light 0) $= white
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  let depth = 1
      phi = 0.35
      beta = 0.0
      spheres = fractal depth phi beta
  depth' <- newIORef depth
  phi' <- newIORef phi
  beta' <- newIORef beta
  spheres' <- newIORef spheres
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextSpheres = spheres'}
                             zoom
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom depth' phi' beta' spheres')
  idleCallback $= Nothing
  putStrLn "*** Apollony fractal ***\n\
        \    To quit, press q.\n\
        \    Scene rotation: e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease depth: h, n\n\
        \    Increase/decrease phi: g, b\n\
        \    Increase/decrease beta: f, v\n\
        \"
  mainLoop
