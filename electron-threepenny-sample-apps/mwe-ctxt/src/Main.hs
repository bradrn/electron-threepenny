module Main where

import System.Environment (getArgs)
import System.IO

import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import           Foreign.JavaScript           (JSObject, root)
import           Foreign.RemotePtr            (addReachable)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [port] <- getArgs
    startGUI defaultConfig {jsPort = Just $ read port} setup

setup :: Window -> UI ()
setup window = do
    rootObj <- liftJSWindow $ return . root
    electron <- callFunction $ ffi "require('electron')" :: UI JSObject
    liftIO $ addReachable rootObj electron
    clickFun <- ffiExport $ runUI window $ runFunction $ ffi "alert('I have been clicked')"
    menu <- callFunction $ ffi "[{label:'Click me',click:%1}]" clickFun :: UI JSObject
    menu' <- callFunction $ ffi "%1.remote.Menu.buildFromTemplate(%2)" electron menu :: UI JSObject
    liftIO $ addReachable electron menu'
    runFunction $ ffi "window.addEventListener('contextmenu', function (e) { e.preventDefault(); %1.popup(%2.remote.getCurrentWindow()); }, false)" menu' electron
    return ()
