{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.UI.GLFW.Pal (
    createWindow, 
    withWindow,
    whileWindow,
    closeOnEscape,
    processEvents, 
    Event(..),
    Events(..),
    ModKey(..),
    onKey,
    onKeyDown, -- Ignores repeating
    onKeyUp,
    onChar,
    whenKeyPressed,
    getWindowViewport, 
    matchModKeys,
    onKeyWithMods,
    -- Lifted
    swapBuffers,
    getWindowSize,
    setWindowSize,
    getFramebufferSize,
    getCursorPos,
    getKey,
    getWindowFocused,
    setCursorInputMode,
    getClipboardString,
    setClipboardString,
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
    -- Need linear
    windowPosToWorldRay,
    getWindowProjection
    ) where

import Graphics.UI.GLFW hiding (
    createWindow, swapBuffers, getWindowSize,
    getCursorPos, getKey, getWindowFocused, setCursorInputMode, 
    getFramebufferSize, setWindowSize, 
    getClipboardString, setClipboardString)
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent.STM

import Control.Monad.Trans
import Control.Exception
import Control.Monad hiding (forM_)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Foldable
import Linear.Extra
import Control.Lens
import Data.List (sort)

data ModKey = ModKeyShift
            | ModKeyControl
            | ModKeyAlt
            | ModKeySuper
            deriving (Eq, Show, Ord)

data Event = Key Key Int KeyState ModifierKeys
           | Character Char
           | MouseButton MouseButton MouseButtonState ModifierKeys
           | MouseCursor Double Double
           | MouseScroll Double Double
           | WindowPos Int Int
           | WindowSize Int Int
           | FramebufferSize Int Int
           | GamepadButton GamepadButton JoystickButtonState
           | GamepadAxes GamepadAllAxes
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
    { gaxLeftStickX   :: !Double
    , gaxLeftStickY   :: !Double
    , gaxTriggers     :: !Double
    , gaxRightStickX  :: !Double
    , gaxRightStickY  :: !Double
    } deriving Show

data Events = Events { esEvents :: TChan Event, esLastPressed :: TVar (Map Joystick (Set GamepadButton)) }

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

    setWindowCloseCallback win . Just $ \_ -> setWindowShouldClose win True
    eventChan <- setupEventChan win
    buttons <- newTVarIO mempty
    return (win, Events eventChan buttons)
    
    
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

processEvents :: MonadIO m => Events -> (Event -> m a) -> m ()
processEvents events action = do

    liftIO (pollJoysticks events)

    liftIO pollEvents
    
    let processNext = (liftIO . atomically . tryReadTChan) (esEvents events) >>= \case
            Just e -> action e >> processNext
            Nothing -> return ()
    processNext


pollJoysticks :: Events -> IO ()
pollJoysticks events = forM_ [Joystick'1, Joystick'2, Joystick'3, Joystick'4] $ \joystick -> do
    let writeEvent = atomically . writeTChan (esEvents events)
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
                                { gaxLeftStickX   = leftStickX
                                , gaxLeftStickY   = leftStickY
                                , gaxTriggers     = triggers
                                , gaxRightStickX  = rightStickX
                                , gaxRightStickY  = rightStickY
                                }
                        writeEvent (GamepadAxes axes)
                        return ()
                    _other -> return ()
            _ -> return ()


-- | Can be plugged into the processEvents function
closeOnEscape :: MonadIO m => Window -> Event -> m ()
closeOnEscape win (Key Key'Escape _ KeyState'Pressed _) = liftIO $ setWindowShouldClose win True
closeOnEscape _   _                                     = return ()

-- | Initializes GLFW and creates a window, and ensures it is cleaned up afterwards
withWindow :: String -> Int -> Int -> ((Window, Events) -> IO c) -> IO c
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

-- | If the event matches the key, run the action.
onKeyDown :: Monad m => Event -> Key -> m () -> m ()
onKeyDown (Key eventKey _ KeyState'Pressed _) key action
    | eventKey == key = action
onKeyDown _ _ _ = return ()

onKey :: Monad m => Event -> Key -> m () -> m ()
onKey keyEvent = onKeyWithMods keyEvent []

modKeysFromBools :: ModifierKeys -> [ModKey]
modKeysFromBools ModifierKeys{..} = sort $
    concat [ [ModKeyShift   | modifierKeysShift]
           , [ModKeyControl | modifierKeysControl]
           , [ModKeyAlt     | modifierKeysAlt]
           , [ModKeySuper   | modifierKeysSuper]
           ]

matchModKeys :: ModifierKeys -> [ModKey] -> Bool
matchModKeys modKeyBools modKeys = modKeysFromBools modKeyBools == sort modKeys

onKeyWithMods :: Monad m => Event -> [ModKey] -> Key ->  m () -> m ()
onKeyWithMods (Key eventKey _ keyState modifierKeyBools) modKeys key action
    |     eventKey == key 
       && keyState `elem` [KeyState'Pressed, KeyState'Repeating]
       && matchModKeys modifierKeyBools modKeys
    = action
onKeyWithMods _ _ _ _ = return ()

onKeyUp :: Monad m => Event -> Key -> m () -> m ()
onKeyUp (Key eventKey _ KeyState'Released _) key action
    | eventKey == key = action
onKeyUp _ _ _ = return ()

onChar :: Monad m => Event -> (Char -> m ()) -> m ()
onChar (Character c) f = f c
onChar _             _ = return ()

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

getFramebufferSize :: MonadIO m => Window -> m (Int, Int)
getFramebufferSize = liftIO . GLFW.getFramebufferSize

-- | Pass this to glViewport to get the correct size on Retina Macs and normal Windows
getWindowViewport :: Num a => MonadIO m => Window -> m (a, a, a, a)
getWindowViewport win = do
    (w, h) <- getFramebufferSize win
    return (0, 0, fromIntegral w, fromIntegral h)

getKey :: MonadIO m => Window -> Key -> m KeyState
getKey win = liftIO . GLFW.getKey win

getCursorPos :: (Fractional t, MonadIO m) => Window -> m (t, t)
getCursorPos win = do
    (x,y) <- liftIO (GLFW.getCursorPos win)
    return (realToFrac x, realToFrac y)

getWindowFocused :: (Functor m, MonadIO m) => Window -> m Bool
getWindowFocused win = (== GLFW.FocusState'Focused) <$> liftIO (GLFW.getWindowFocused win)

setCursorInputMode :: MonadIO m => Window -> CursorInputMode -> m ()
setCursorInputMode win = liftIO . GLFW.setCursorInputMode win


-- | Use the aspect ratio from the window to get a proper projection
getWindowProjection :: (Floating a, MonadIO m) => Window -> a -> a -> a -> m (M44 a)
getWindowProjection win fov near far = do
    (w,h) <- getWindowSize win
    return $ perspective fov (fromIntegral w / fromIntegral h) near far

-- By Colin Barrett, originally from vr-pal
windowPosToWorldRay :: (RealFloat a, Conjugate a, Epsilon a, MonadIO m) 
                    => Window 
                    -> M44 a
                    -> Pose a 
                    -> (a, a)
                    -> m (V3 a, V3 a)
windowPosToWorldRay win proj pose coord = do
  (w, h) <- getWindowSize win
  let (xNDC, yNDC) = win2Ndc coord (w,h)
      start = ndc2Wld (V4 xNDC yNDC (-1.0) 1.0)
      end   = ndc2Wld (V4 xNDC yNDC 0.0    1.0)
      dir   = normalize (end ^-^ start)
  return (start, dir ^* 1000.0)

  where -- Converts from window coordinates (origin top-left) to normalized device coordinates
    win2Ndc (x, y) (w, h) = 
      let h' = fromIntegral h
      in ((((x / fromIntegral w) - 0.5) * 2.0), ((((h' - y) / h') - 0.5) * 2.0))
    -- Converts from normalized device coordinates to world coordinates
    ndc2Wld i = hom2Euc (invViewProj !* i)
    -- Converts from homogeneous coordinates to Euclidean coordinates
    hom2Euc v = (v ^/ (v ^. _w)) ^. _xyz
    invViewProj = safeInv44 (proj !*! viewMatrixFromPose pose)
