{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Electron.Threepenny.BrowserWindow.BrowserWindow
    ( BrowserWindow
    , getFocusedWindow) where

import Electron.Threepenny.Core (JSModule)
import Graphics.UI.Threepenny.Core
import Foreign.JavaScript (JSObject)

newtype BrowserWindow = BrowserWindow JSObject deriving (ToJS)

getFocusedWindow :: JSModule "electron" -> UI BrowserWindow
getFocusedWindow electron = BrowserWindow <$> (callFunction $ ffi "%1.remote.BrowserWindow.getFocusedWindow()" electron :: UI JSObject)
