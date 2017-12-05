{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Electron.Threepenny.BrowserWindow
       ( BrowserWindow
       , getFocusedWindow
       , destroy
       , focus
       , blur
       , isFocused
       , isDestroyed
       , show_
       , showInactive
       , hide
       , isVisible
       , isModal
       , maximize
       , unmaximize
       , isMaximized
       , minimize
       , restore
       , isMinimized
       , setFullScreen
       , isFullScreen
       , setAspectRatio
       , previewFile
       , closeFilePreview
       ) where

import           Data.Aeson        (Value (..), Object)
import           Data.Maybe        (maybe, fromJust)
import           Data.Scientific   (toBoundedInteger, toRealFloat)
import           Data.Text         (pack)
import qualified Data.HashMap.Lazy as HMap

import Electron.Threepenny.Core
import Electron.Threepenny.JSRep
import Foreign.JavaScript (JSObject)
import Graphics.UI.Threepenny.Core

newtype BrowserWindow = BrowserWindow JSObject deriving (ToJS)

getFocusedWindow :: JSModule "electron" -> UI BrowserWindow
getFocusedWindow electron = BrowserWindow <$> (callFunction $ ffi "%1.remote.BrowserWindow.getFocusedWindow()" electron :: UI JSObject)

destroy      = mkBrowserWindowMethodRun  "destroy"
close        = mkBrowserWindowMethodRun  "close"
focus        = mkBrowserWindowMethodRun  "focus"
blur         = mkBrowserWindowMethodRun  "blur"
isFocused    = mkBrowserWindowMethodPred "isFocused"
isDestroyed  = mkBrowserWindowMethodPred "isDestroyed"
show_        = mkBrowserWindowMethodRun  "show"
showInactive = mkBrowserWindowMethodRun  "showInactive"
hide         = mkBrowserWindowMethodRun  "hide"
isVisible    = mkBrowserWindowMethodPred "isVisible"
isModal      = mkBrowserWindowMethodPred "isModal"
maximize     = mkBrowserWindowMethodRun  "maximize"
unmaximize   = mkBrowserWindowMethodRun  "unmaximize"
isMaximized  = mkBrowserWindowMethodPred "isMaximized"
minimize     = mkBrowserWindowMethodRun  "minimize"
restore      = mkBrowserWindowMethodRun  "restore"
isMinimized  = mkBrowserWindowMethodPred "isMinimized"

setFullScreen :: BrowserWindow -> Bool -> UI ()
setFullScreen = mkBrowserWindowMethodArg "setFullScreen"

isFullScreen = mkBrowserWindowMethodPred "isFullScreen"

setAspectRatio :: BrowserWindow -> Float -> Maybe (Float, Float) -> UI ()
setAspectRatio win aspectRatio extraSize = runFunction $
    case extraSize of
        Nothing -> ffi "%1.setAspectRatio(%2)" win aspectRatio
        Just (width, height) -> ffi "%1.setAspectRatio(%2, {width: %3, height: %4})" win aspectRatio width height

previewFile :: BrowserWindow -> FilePath -> Maybe String -> UI ()
previewFile win path displayName = runFunction $
    case displayName of
        Nothing -> ffi "%1.previewFile(%2)" path
        Just displayName' -> ffi "%1.previewFile(%2, %3)" path displayName'

closeFilePreview = mkBrowserWindowMethodRun "closeFilePreview"

setBounds :: BrowserWindow -> Rectangle -> Maybe Bool -> UI ()
setBounds win rect animate = runFunction $
    case animate of
        Nothing -> ffi "%1.previewFile(%2)" (jsRep rect)
        Just animate' -> ffi "%1.previewFile(%2, %3)" (jsRep rect) animate'

getBounds   :: BrowserWindow -> UI Rectangle
getBounds win = do
    value   :: Value <- callFunction $ ffi "%1.getBounds()" win
    case getRect value of
        Just rect          -> return rect
        Nothing            -> error "The impossible happened! An Electron Rectangle object was invalid. Please report this to the developer as an issue."
  where
    getRect :: Value       -> Maybe Rectangle
    getRect (Object map)      = do
        x          <- getNumRecord map "x"
        y          <- getNumRecord map "y"
        width      <- getNumRecord map "width"
        height     <- getNumRecord map "height"
        return Rectangle{..}
    getRect _                 = Nothing

    getNum :: Value -> Maybe Int
    getNum (Number s) = toBoundedInteger s
    getNum _          = Nothing

    getNumRecord :: Object -> String -> Maybe Int
    getNumRecord map r = do
        entry <- HMap.lookup (pack r) map
        getNum entry

--TODO: setContentBounds, getContentBounds, setSize, getSize, setContentSize, getContentSize, setMinimumSize, getMinimumSize,
--      setMaximumSize, getMaximumSize

setResizable :: BrowserWindow -> Bool -> UI ()
setResizable = mkBrowserWindowMethodArg "setResizable"
isResizable  = mkBrowserWindowMethodPred "isResizable"

setMovable :: BrowserWindow -> Bool -> UI ()
setMovable = mkBrowserWindowMethodArg "setMovable"
isMovable  = mkBrowserWindowMethodPred "isMovable"

setMinimizable :: BrowserWindow -> Bool -> UI ()
setMinimizable = mkBrowserWindowMethodArg "setMinimizable"
isMinimizable  = mkBrowserWindowMethodPred "isMinimizable"

setMaximizable :: BrowserWindow -> Bool -> UI ()
setMaximizable = mkBrowserWindowMethodArg "setMaximizable"
isMaximizable  = mkBrowserWindowMethodPred "isMaximizable"

setFullScreenable :: BrowserWindow -> Bool -> UI ()
setFullScreenable = mkBrowserWindowMethodArg "setFullScreenable"
isFullScreenable  = mkBrowserWindowMethodPred "isFullScreenable"

setClosable :: BrowserWindow -> Bool -> UI ()
setClosable = mkBrowserWindowMethodArg "setClosable"
isClosable  = mkBrowserWindowMethodPred "isClosable"

setAlwaysOnTop :: BrowserWindow -> Bool -> Maybe MacWindowLevel -> Maybe Int -> UI ()
setAlwaysOnTop win flag level relativeLevel = runFunction $ ffi js win (jsRep $ fromJust level) (fromJust relativeLevel)
  where
    js = "%1.setAlwaysOnTop(%2" ++ maybe "" (const ",%3") level ++ maybe "" (const ",%4") level

isAlwaysOnTop = mkBrowserWindowMethodPred "isAlwaysOnTop"

--TODO: `center` etc.

data Rectangle = Rectangle {x :: Int, y :: Int, width :: Int, height :: Int}

instance JSRep Rectangle where
    jsRep Rectangle{..} =
        "{x:" ++ show x ++ ", y:" ++ show y ++ ", width:" ++ show width ++ ", height:" ++ show height ++ "}"

data MacWindowLevel = Normal
                    | Floating
                    | TornOffMenu
                    | ModalPanel
                    | MainMenu
                    | Status
                    | PopUpMenu
                    | ScreenSaver
                    | Dock
{-# DEPRECATED Dock "This window level is deprecated" #-}

instance JSRep MacWindowLevel where
  jsRep Normal = "normal"
  jsRep Floating = "floating"
  jsRep TornOffMenu = "torn-off-menu"
  jsRep ModalPanel = "modal-panel"
  jsRep MainMenu = "main-menu"
  jsRep Status = "status"
  jsRep PopUpMenu = "pop-up-menu"
  jsRep ScreenSaver = "screen-saver"
  jsRep Dock = "dock"

mkBrowserWindowMethodRun :: String -> (BrowserWindow -> UI ())
mkBrowserWindowMethodRun m = \win -> runFunction $ ffi ("%1." ++ m ++ "()") win

mkBrowserWindowMethodPred :: String -> (BrowserWindow -> UI Bool)
mkBrowserWindowMethodPred m = \win -> do
    val :: Value <- callFunction $ ffi ("%1." ++ m ++ "()") win
    case val of
        Bool b -> return b
        _ -> error "The impossible happened! An Electron method was documented as returning a boolean, but it returned something else. Please report this to the developer as an issue."

mkBrowserWindowMethodArg :: ToJS a => String -> (BrowserWindow -> a -> UI ())
mkBrowserWindowMethodArg m = \win arg -> runFunction $ ffi ("%1." ++ m ++ "(%2)") win arg
