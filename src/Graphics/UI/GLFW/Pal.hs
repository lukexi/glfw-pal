{-# LANGUAGE LambdaCase #-}
module Graphics.UI.GLFW.Pal (
    createWindow, 
    withWindow,
    whileWindow,
    closeOnEscape,
    processEvents, 
    Event(..),
    keyDown,
    -- Lifted
    swapBuffers,
    getWindowSize,
    getFramebufferSize,
    getCursorPos,
    getKey,
    getWindowFocused,
    setCursorInputMode,
    -- Re-exports
    Window,
    Key(..),
    KeyState(..),
    MouseButton(..),
    MouseButtonState(..),
    CursorInputMode(..)
    ) where

import Graphics.UI.GLFW hiding (
    createWindow, swapBuffers, getWindowSize,
    getCursorPos, getKey, getWindowFocused, setCursorInputMode, 
    getFramebufferSize)
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent.STM

import Control.Monad.Trans
import Control.Exception

data Event = Key Key Int KeyState ModifierKeys
           | Character Char
           | MouseButton MouseButton MouseButtonState ModifierKeys
           | MouseCursor Double Double
           | MouseScroll Double Double
           | WindowPos Int Int
           | WindowSize Int Int
           | FramebufferSize Int Int
           deriving Show

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

    setWindowCloseCallback win . Just $ \_ -> setWindowShouldClose win True
    events <- setupEventChan win
    return (win, events)
    
    
    where
    
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

processEvents :: MonadIO m => TChan a -> (a -> m a1) -> m ()
processEvents events action = do
    liftIO pollEvents
    let processNext = (liftIO . atomically . tryReadTChan) events >>= \case
            Just e -> action e >> processNext
            Nothing -> return ()
    processNext

-- | Can be plugged into the processEvents function
closeOnEscape :: MonadIO m => Window -> Event -> m ()
closeOnEscape win (Key Key'Escape _ KeyState'Pressed _) = liftIO $ setWindowShouldClose win True
closeOnEscape _   _                                     = return ()

-- | Initializes GLFW and creates a window, and ensures it is cleaned up afterwards
withWindow :: String -> Int -> Int -> ((Window, TChan Event) -> IO c) -> IO c
withWindow name width height action = 
    bracket (createWindow name width height) 
            -- We must make the current context Nothing or we won't be able
            -- to create any more GL windows (probably a bug in GLFW)
            (\(win, _) -> makeContextCurrent Nothing >> destroyWindow win >> GLFW.terminate)
            action

-- | Like 'forever', but checks if the windowShouldClose flag 
-- is set on each loop and returns if so.
whileWindow :: MonadIO m => Window -> m a -> m ()
whileWindow win action = liftIO (windowShouldClose win) >>= \case
    True  -> return ()
    False -> action >> whileWindow win action

keyDown :: Monad m => Key -> Event -> m () -> m ()
keyDown key (Key eventKey _ KeyState'Pressed _) action
    | eventKey == key = action
keyDown _ _ _ = return ()

-- Lifted versions of GLFW functions
swapBuffers :: MonadIO m => Window -> m ()
swapBuffers = liftIO . GLFW.swapBuffers

getWindowSize :: MonadIO m => Window -> m (Int, Int)
getWindowSize = liftIO . GLFW.getWindowSize

getFramebufferSize :: MonadIO m => Window -> m (Int, Int)
getFramebufferSize = liftIO . GLFW.getFramebufferSize

getKey :: MonadIO m => Window -> Key -> m KeyState
getKey win = liftIO . GLFW.getKey win

getCursorPos :: (Fractional t, MonadIO m) => Window -> m (t, t)
getCursorPos win = do
    (x,y) <- liftIO (GLFW.getCursorPos win)
    return (realToFrac x, realToFrac y)

getWindowFocused :: MonadIO m => Window -> m Bool
getWindowFocused win = (== GLFW.FocusState'Focused) <$> liftIO (GLFW.getWindowFocused win)

setCursorInputMode :: MonadIO m => Window -> CursorInputMode -> m ()
setCursorInputMode win = liftIO . GLFW.setCursorInputMode win