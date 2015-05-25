{-# LANGUAGE LambdaCase #-}
import Graphics.UI.GLFW.Pal
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Control.Monad

main :: IO ()
main = withWindow "GLFW Pal" 640 480 $ \(win, eventsChan) -> do
    glClearColor 1 1 0 1

    whileWindow win $ do
        processEvents eventsChan (closeOnEscape win)
        glClear GL_COLOR_BUFFER_BIT
        swapBuffers win