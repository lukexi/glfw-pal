{-# LANGUAGE LambdaCase #-}
module Graphics.UI.GLFW.Pal (
    createWindow, 
    withWindow,
    processEvents, 
    swapBuffers, 
    Event(..),
    ) where

import Graphics.UI.GLFW hiding (createWindow)
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent.STM

import Control.Monad.Trans
import Control.Exception

createWindow :: String -> Int -> Int -> IO (GLFW.Window, TChan Event)
createWindow windowName desiredW desiredH = do
    setErrorCallback (Just (\err string -> 
        putStrLn $ "GLFW Error: " ++ string ++ show err))
    _ <- GLFW.init

    windowHint $ WindowHint'ClientAPI ClientAPI'OpenGL
    windowHint $ WindowHint'OpenGLForwardCompat True
    windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
    windowHint $ WindowHint'ContextVersionMajor 4
    windowHint $ WindowHint'ContextVersionMinor 1
    windowHint $ WindowHint'sRGBCapable True
    
    Just win <- GLFW.createWindow desiredW desiredH windowName Nothing Nothing
    
    makeContextCurrent (Just win)

    swapInterval 1
    events <- setupEventChan win
    return (win, events)

processEvents :: MonadIO m => TChan a -> (a -> m a1) -> m ()
processEvents events action = do
    liftIO pollEvents
    let processNext = (liftIO . atomically . tryReadTChan) events >>= \case
                Just e -> action e >> processNext
                Nothing -> return ()
    processNext

withWindow :: String -> Int -> Int -> ((Window, TChan Event) -> IO c) -> IO c
withWindow name width height action = 
    bracket (createWindow name width height) 
            (\(win, _) -> destroyWindow win >> terminate ) 
            action

setupEventChan :: Window -> IO (TChan Event)
setupEventChan win = do
    eventChan <- newTChanIO
    let writeEvent = atomically . writeTChan eventChan

    setKeyCallback win          . Just $ \_ key code state mods -> writeEvent (Key key code state mods)
    setCharCallback win         . Just $ \_ char                -> writeEvent (Character char)
    setMouseButtonCallback win  . Just $ \_ button state mods   -> writeEvent (MouseButton button state mods)
    setCursorPosCallback win    . Just $ \_ x y                 -> writeEvent (MouseCursor x y)
    setScrollCallback win       . Just $ \_ x y                 -> writeEvent (MouseScroll x y)

    setWindowPosCallback       win . Just $ \_ x y -> writeEvent (WindowPos x y)
    setWindowSizeCallback      win . Just $ \_ x y -> writeEvent (WindowSize x y)
    setFramebufferSizeCallback win . Just $ \_ w h -> writeEvent (FramebufferSize w h)

    return eventChan


data Event = Key Key Int KeyState ModifierKeys
           | Character Char
           | MouseButton MouseButton MouseButtonState ModifierKeys
           | MouseCursor Double Double
           | MouseScroll Double Double
           | WindowPos Int Int
           | WindowSize Int Int
           | FramebufferSize Int Int
           deriving Show