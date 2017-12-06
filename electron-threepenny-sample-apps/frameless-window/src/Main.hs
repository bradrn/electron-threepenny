{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Bool          (bool)
import Data.Maybe         (fromJust)
import System.Environment (getArgs)
import System.IO

import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import           Foreign.JavaScript           (JSObject)

import Electron.Threepenny.Core
import Electron.Threepenny.BrowserWindow

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [port] <- getArgs
    putStrLn port
    startGUI defaultConfig { jsPort = Just $ read port
                           , jsStatic = Just "static"
                           , jsCustomHTML = Just "index.html"
                           } setup

setup :: Window -> UI ()
setup window = do
    electron :: JSModule "electron" <- require
    win <- getFocusedWindow

    topCheckbox    <- getElementById' window "top-box"
    bottomCheckbox <- getElementById' window "bottom-box"
    leftCheckbox   <- getElementById' window "left-box"
    rightCheckbox  <- getElementById' window "right-box"

    topCheckboxVal    <- stepper False $ UI.checkedChange topCheckbox
    bottomCheckboxVal <- stepper False $ UI.checkedChange bottomCheckbox
    leftCheckboxVal   <- stepper False $ UI.checkedChange leftCheckbox
    rightCheckboxVal  <- stepper False $ UI.checkedChange rightCheckbox

    element topCheckbox    # sink UI.enabled (notOr <$> leftCheckboxVal <*> rightCheckboxVal)
    element bottomCheckbox # sink UI.enabled (notOr <$> leftCheckboxVal <*> rightCheckboxVal)
    element leftCheckbox  # sink UI.enabled (notOr <$> topCheckboxVal <*> bottomCheckboxVal)
    element rightCheckbox # sink UI.enabled (notOr <$> topCheckboxVal <*> bottomCheckboxVal)

    initCheckbox topCheckbox "top-titlebar" "top-titlebar.png" "Top Titlebar"
    initCheckbox bottomCheckbox "bottom-titlebar" "bottom-titlebar.png" "Bottom Titlebar"
    initCheckbox leftCheckbox "left-titlebar" "left-titlebar.png" "Left Titlebar"
    initCheckbox rightCheckbox "right-titlebar" "right-titlebar.png" "Right Titlebar"

    closeButton <- getElementById' window "close-window-button"
    minimizeButton <- getElementById' window "minimize-window-button"
    maximizeButton <- getElementById' window "maximize-window-button"
    unmaximizeButton <- getElementById' window "unmaximize-window-button"
    toggleButton <- getElementById' window "toggle-window-button"
    maxminButton <- getElementById' window "maxmin-window-button"
    minButton <- getElementById' window "min"
    maxButton <- getElementById' window "max"
    exitButton <- getElementById' window "exit"

    onEvent (UI.click closeButton)      $ const $ close win
    onEvent (UI.click minimizeButton)   $ const $ runFunction $ ffi "%1.minimize()" win
    onEvent (UI.click maximizeButton)   $ const $ runFunction $ ffi "%1.maximize()"
    onEvent (UI.click unmaximizeButton) $ const $ runFunction $ ffi "%1.unmaximize()"
    onEvent (UI.click toggleButton)     $ const $ do
        isFullScreen <- fmap (==1) $ (callFunction $ ffi "Number(%1.isFullScreen())" win :: UI Int)
        runFunction $ ffi "%1.setFullScreen(%2)" win (not isFullScreen)

    runOnFocus <- ffiExport $ runUI window $ do
        runFunction $ ffi "console.log('focus')"
        focusTitlebars True
    runFunction $ ffi "window.onfocus = %1" runOnFocus
    runOnBlur <- ffiExport $ runUI window $ do
        runFunction $ ffi "console.log('blur')"
        focusTitlebars False
    runFunction $ ffi "window.onblur = %1" runOnBlur
    runOnResize <- ffiExport $ runUI window updateContentStyle
    runFunction $ ffi "window.onresize = %1" runOnResize
    return ()

  where
    notOr p q = not (p || q)

    initCheckbox box name icon text =
        onEvent (UI.checkedChange box) $ bool (addTitlebar name icon text) (removeTitlebar name)

    getElementById' window id = (fmap fromJust) $ getElementById window id

focusTitlebars :: Bool -> UI ()
focusTitlebars = undefined

updateContentStyle :: UI ()
updateContentStyle = undefined

addTitlebar :: String -> String -> String -> UI ()
addTitlebar = undefined

removeTitlebar :: String -> UI ()
removeTitlebar = undefined
