import Graphics.UI.GLFW.Pal
import Graphics.GL
import Control.Monad

main :: IO ()
main = withWindow "GLFW Pal" 640 480 $ \(win, eventsChan) -> do
    glClearColor 1 1 0 1
    forever $ do
        processEvents eventsChan $ return
        glClear GL_COLOR_BUFFER_BIT
        swapBuffers win