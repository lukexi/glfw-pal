{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.UI.GLFW.Pal (
    module Graphics.UI.GLFW.Pal,
    -- Re-exports
    swapInterval,
    Window,
    Key(..),
    KeyState(..),
    MouseButton(..),
    MouseButtonState(..),
    CursorInputMode(..),
    GamepadAllAxes(..),
    GamepadButton(..),
    ModifierKeys(..)
    ) where

import Graphics.UI.GLFW hiding (
    createWindow, swapBuffers, getWindowSize,
    getCursorPos, getKey, getMouseButton, getWindowFocused, setCursorInputMode,
    getFramebufferSize, setWindowSize,
    getClipboardString, setClipboardString, hideWindow, iconifyWindow,
    makeContextCurrent)
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent.STM
import GHC.IO.Handle
import System.IO
import Control.Monad

import Control.Monad.Trans
import Control.Exception
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Linear.Extra
import Control.Lens
import Data.List (sort)
import Data.IORef

data ModKey = ModKeyShift
            | ModKeyControl
            | ModKeyAlt
            | ModKeySuper
            deriving (Eq, Show, Ord)

data Event = KeyboardKey Key Int KeyState ModifierKeys
           | Character Char
           | MouseButton MouseButton MouseButtonState ModifierKeys
           | MouseCursor Float Float
           | MouseScroll Float Float
           | WindowPos Int Int
           | WindowSize Int Int
           | FramebufferSize Int Int
           | GamepadButton GamepadButton JoystickButtonState
           | GamepadAxes GamepadAllAxes
           | WindowRefresh
           deriving Show

-- These are defined in the order we expect to find them in getJoystickButtons
data GamepadButton = GamepadButtonA
                   | GamepadButtonB
                   | GamepadButtonX
                   | GamepadButtonY
                   | GamepadButtonLeftShoulder
                   | GamepadButtonRightShoulder
                   | GamepadButtonBack
                   | GamepadButtonStart
                   | GamepadButtonLeftStick
                   | GamepadButtonRightStick
                   | GamepadButtonDPadUp
                   | GamepadButtonDPadRight
                   | GamepadButtonDPadDown
                   | GamepadButtonDPadLeft
                   deriving (Show, Eq, Ord, Enum, Bounded)

data GamepadAllAxes = GamepadAllAxes
    { gaxLeftStickX   :: !Float
    , gaxLeftStickY   :: !Float
    , gaxTriggers     :: !Float
    , gaxRightStickX  :: !Float
    , gaxRightStickY  :: !Float
    } deriving Show

data Events = Events
    { esEvents :: IORef [Event]
    , esLastPressed :: TVar (Map Joystick (Set GamepadButton))
    }

-- | Use on Windows at the very start of main to
-- redirect any output to the console to the given log file
-- Use this when using
-- ghc-options: -optl-mwindows
-- to hide the console window in an end-user application.
suppressConsole :: FilePath -> IO ()
suppressConsole fileName = do
    allOutput <- openFile fileName WriteMode
    hDuplicateTo allOutput stdout
    hDuplicateTo allOutput stderr
    hDuplicateTo allOutput stdin

createWindow :: String -> Int -> Int -> IO (Window, Events)
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

    swapInterval 0

    eventChan <- setupEventChan win
    buttons <- newTVarIO mempty
    return (win, Events eventChan buttons)


    where

        setupEventChan :: Window -> IO (IORef [Event])
        setupEventChan win = do
            eventsRef <- newIORef []
            let writeEvent event = modifyIORef' eventsRef (event:)

            setWindowCloseCallback     win . Just $ \_                     -> setWindowShouldClose win True

            setKeyCallback             win . Just $ \_ key code state mods -> writeEvent (KeyboardKey key code state mods)
            setCharCallback            win . Just $ \_ char                -> writeEvent (Character char)
            setMouseButtonCallback     win . Just $ \_ button state mods   -> writeEvent (MouseButton button state mods)
            setCursorPosCallback       win . Just $ \_ x y                 -> writeEvent (MouseCursor (realToFrac x) (realToFrac y))
            setScrollCallback          win . Just $ \_ x y                 -> writeEvent (MouseScroll (realToFrac x) (realToFrac y))

            setWindowPosCallback       win . Just $ \_ x y                 -> writeEvent (WindowPos x y)
            setWindowRefreshCallback   win . Just $ \_                     -> writeEvent (WindowRefresh)
            setWindowSizeCallback      win . Just $ \_ x y                 -> writeEvent (WindowSize x y)
            setFramebufferSizeCallback win . Just $ \_ w h                 -> writeEvent (FramebufferSize w h)

            return eventsRef

gatherEvents :: MonadIO m => Events -> m [Event]
gatherEvents Events{..} = liftIO $ do

    writeIORef esEvents []

    -- FIXME: was causing 8ms pauses, so disabling joystick support
    --pollJoysticks events
    pollEvents
    events <- reverse <$> readIORef esEvents

    return events


pollJoysticks :: Events -> IO ()
pollJoysticks events = forM_ [Joystick'1, Joystick'2, Joystick'3, Joystick'4] $ \joystick -> do
    let writeEvent event = modifyIORef' (esEvents events) (event:)
    joystickIsPresent <- liftIO $ joystickPresent joystick
    when joystickIsPresent $
        getJoystickButtons joystick >>= \case
            -- Expect 14 buttons for Xbox 360 controller
            Just states@[_, _, _, _, _, _, _, _, _, _, _, _, _, _] -> do

                -- let buttons = [a,b,x,y, leftShoulder, rightShoulder, back, start, leftStick, rightStick, up, right, down, left]
                -- Turn the list of states into a list of pressed buttons
                let buttons = [minBound..maxBound] :: [GamepadButton]
                    pressed = Set.fromList . map fst . filter (\(_,state) -> state == JoystickButtonState'Pressed) $ zip buttons states
                -- Get the old pressed state and write the new one
                oldPressed <- atomically $ do
                    oldPressed <- readTVar (esLastPressed events)
                    writeTVar (esLastPressed events) (Map.insert joystick pressed oldPressed)
                    return (fromMaybe mempty $ Map.lookup joystick oldPressed)
                -- Diff the old and the new pressed states to find pressed and released events
                let justReleased = oldPressed Set.\\ pressed
                    justPressed  = pressed Set.\\ oldPressed
                -- Write the events to the events channel
                forM_ justPressed  $ \button -> writeEvent (GamepadButton button JoystickButtonState'Pressed)
                forM_ justReleased $ \button -> writeEvent (GamepadButton button JoystickButtonState'Released)
                -- print "hi"
                getJoystickAxes joystick >>= \case
                    Just [leftStickX, leftStickY, triggers, rightStickX, rightStickY] -> do
                        let axes = GamepadAllAxes
                                { gaxLeftStickX   = realToFrac leftStickX
                                , gaxLeftStickY   = realToFrac leftStickY
                                , gaxTriggers     = realToFrac triggers
                                , gaxRightStickX  = realToFrac rightStickX
                                , gaxRightStickY  = realToFrac rightStickY
                                }
                        writeEvent (GamepadAxes axes)
                        return ()
                    _other -> return ()
            _ -> return ()


-- | Can be plugged into the processEvents function
closeOnEscape :: MonadIO m => Window -> Event -> m ()
closeOnEscape win (KeyboardKey Key'Escape _ KeyState'Pressed _) = liftIO $ setWindowShouldClose win True
closeOnEscape _   _                                     = return ()

-- | Initializes GLFW and creates a window, and ensures it is cleaned up afterwards
withWindow :: String -> Int -> Int -> ((Window, Events) -> IO a) -> IO a
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

modKeysFromBools :: ModifierKeys -> [ModKey]
modKeysFromBools ModifierKeys{..} = sort $
    concat [ [ModKeyShift   | modifierKeysShift]
           , [ModKeyControl | modifierKeysControl]
           , [ModKeyAlt     | modifierKeysAlt]
           , [ModKeySuper   | modifierKeysSuper]
           ]

matchModKeys :: ModifierKeys -> [ModKey] -> Bool
matchModKeys modKeyBools modKeys = modKeysFromBools modKeyBools == sort modKeys

ifKeyWithMods :: Monad m => a -> Event -> [ModKey] -> Key ->  m a -> m a
ifKeyWithMods _ (KeyboardKey eventKey _ keyState modifierKeyBools) modKeys key action
    |     eventKey == key
       && keyState `elem` [KeyState'Pressed, KeyState'Repeating]
       && matchModKeys modifierKeyBools modKeys
    = action
ifKeyWithMods a _ _ _ _ = return a

ifKeyUp :: Monad m => a -> Event -> Key -> m a -> m a
ifKeyUp _ (KeyboardKey eventKey _ KeyState'Released _) key action
    | eventKey == key = action
ifKeyUp a _ _ _ = return a

ifChar :: Monad m => a -> Event -> (Char -> m a) -> m a
ifChar _ (Character c) f = f c
ifChar a _             _ = return a

-- | If the event matches the key, run the action.
ifKeyDown :: Monad m => a -> Event -> Key -> m a -> m a
ifKeyDown _ (KeyboardKey eventKey _ KeyState'Pressed _) key action
    | eventKey == key = action
ifKeyDown a _ _ _ = return a

ifKey :: Monad m => a -> Event -> Key -> m a -> m a
ifKey a keyEvent = ifKeyWithMods a keyEvent []




onKeyWithMods :: Monad m => Event -> [ModKey] -> Key -> m () -> m ()
onKeyWithMods = ifKeyWithMods ()

onKeyUp :: Monad m => Event -> Key -> m () -> m ()
onKeyUp = ifKeyUp ()

onChar :: Monad m => Event -> (Char -> m ()) -> m ()
onChar = ifChar ()

-- | If the event matches the key, run the action.
onKeyDown :: Monad m => Event -> Key -> m () -> m ()
onKeyDown = ifKeyDown ()

onKey :: Monad m => Event -> Key -> m () -> m ()
onKey = ifKey ()


onScroll :: (Monad m) => Event -> (Float -> Float -> m ()) -> m ()
onScroll (MouseScroll x y) f = f (realToFrac x) (realToFrac y)
onScroll _                 _ = return ()

onCursor :: (Monad m) => Event -> (Float -> Float -> m ()) -> m ()
onCursor (MouseCursor x y) f = f (realToFrac x) (realToFrac y)
onCursor _                 _ = return ()

onMouseDown :: Monad m => Event -> (MouseButton -> m ()) -> m ()
onMouseDown (MouseButton b MouseButtonState'Pressed _) f = f b
onMouseDown _                                          _ = return ()


onMouseUp :: Monad m => Event -> (MouseButton -> m ()) -> m ()
onMouseUp (MouseButton b MouseButtonState'Released _) f = f b
onMouseUp _                                           _ = return ()

whenMouseDown :: MonadIO m => Window -> MouseButton -> m () -> m ()
whenMouseDown win button action = getMouseButton win button >>= \case
    MouseButtonState'Pressed -> action
    _                        -> return ()


whenKeyPressed :: MonadIO m => Window -> Key -> m () -> m ()
whenKeyPressed win key action = getKey win key >>= \case
    KeyState'Pressed -> action
    _                -> return ()

-- Lifted versions of GLFW functions
swapBuffers :: MonadIO m => Window -> m ()
swapBuffers = liftIO . GLFW.swapBuffers

getWindowSize :: MonadIO m => Window -> m (Int, Int)
getWindowSize = liftIO . GLFW.getWindowSize

getClipboardString :: MonadIO m => Window -> m (Maybe String)
getClipboardString = liftIO . GLFW.getClipboardString

setClipboardString :: MonadIO m => Window -> String -> m ()
setClipboardString win = liftIO . GLFW.setClipboardString win

setWindowSize :: MonadIO m => Window -> Int -> Int -> m ()
setWindowSize win w h = liftIO (GLFW.setWindowSize win w h)

setWindowPosition :: MonadIO m => Window -> Int -> Int -> m ()
setWindowPosition win x y = liftIO (GLFW.setWindowPos win x y)

getFramebufferSize :: MonadIO m => Window -> m (Int, Int)
getFramebufferSize = liftIO . GLFW.getFramebufferSize

-- | Pass this to glViewport to get the correct size on Retina Macs and normal Windows
getWindowViewport :: Num a => MonadIO m => Window -> m (a, a, a, a)
getWindowViewport win = do
    (w, h) <- getFramebufferSize win
    return (0, 0, fromIntegral w, fromIntegral h)

getKey :: MonadIO m => Window -> Key -> m KeyState
getKey win = liftIO . GLFW.getKey win

getMouseButton :: MonadIO m => Window -> MouseButton -> m MouseButtonState
getMouseButton win = liftIO . GLFW.getMouseButton win

getCursorPos :: (MonadIO m) => Window -> m (Float, Float)
getCursorPos win = do
    (x,y) <- liftIO (GLFW.getCursorPos win)
    return (realToFrac x, realToFrac y)

getWindowFocused :: (MonadIO m) => Window -> m Bool
getWindowFocused win = (== GLFW.FocusState'Focused) <$> liftIO (GLFW.getWindowFocused win)

setCursorInputMode :: MonadIO m => Window -> CursorInputMode -> m ()
setCursorInputMode win = liftIO . GLFW.setCursorInputMode win

hideWindow :: (MonadIO m) => Window -> m ()
hideWindow = liftIO . GLFW.hideWindow

iconifyWindow :: (MonadIO m) => Window -> m ()
iconifyWindow = liftIO . GLFW.iconifyWindow

showWindow :: (MonadIO m) => Window -> m ()
showWindow = liftIO . GLFW.showWindow

restoreWindow :: (MonadIO m) => Window -> m ()
restoreWindow = liftIO . GLFW.restoreWindow

makeContextCurrent :: (MonadIO m) => Maybe Window -> m ()
makeContextCurrent = liftIO . GLFW.makeContextCurrent

-- | Use the aspect ratio from the window to get a proper projection
getWindowProjection :: (Floating a, MonadIO m) => Window -> a -> a -> a -> m (M44 a)
getWindowProjection win fov near far = do
    (w,h) <- getWindowSize win
    return $ perspective fov (fromIntegral w / fromIntegral h) near far

-- By Colin Barrett, originally from vr-pal
windowPosToWorldRay :: (MonadIO m)
                    => Window
                    -> M44 Float
                    -> Pose Float
                    -> (Float, Float)
                    -> m (Ray Float)
windowPosToWorldRay win proj pose coord = do
    (w, h) <- getWindowSize win
    let (xNDC, yNDC) = win2Ndc coord (w,h)
        start = ndc2Wld (V4 xNDC yNDC (-1.0) 1.0)
        end   = ndc2Wld (V4 xNDC yNDC 0.0    1.0)
        dir   = normalize (end ^-^ start)
    return (Ray start dir)

    where -- Converts from window coordinates (origin top-left) to normalized device coordinates
      win2Ndc (x, y) (w, h) =
        let h' = fromIntegral h
        in ((((x / fromIntegral w) - 0.5) * 2.0), ((((h' - y) / h') - 0.5) * 2.0))
      -- Converts from normalized device coordinates to world coordinates
      ndc2Wld i = hom2Euc (invViewProj !* i)
      -- Converts from homogeneous coordinates to Euclidean coordinates
      hom2Euc v = (v ^/ (v ^. _w)) ^. _xyz
      invViewProj = inv44 (proj !*! viewMatrixFromPose pose)

cursorPosToWorldRay :: (MonadIO m)
                    => Window
                    -> M44 Float
                    -> Pose Float
                    -> m (Ray Float)
cursorPosToWorldRay win proj pose = do
    cursorPos <- getCursorPos win
    windowPosToWorldRay win proj pose cursorPos
