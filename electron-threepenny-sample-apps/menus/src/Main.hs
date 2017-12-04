{-# LANGUAGE DataKinds #-}

module Main where

import System.Environment (getArgs)
import System.IO

import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import           Foreign.JavaScript           (JSObject)

import Electron.Threepenny.Core
import Electron.Threepenny.Menu
import Electron.Threepenny.Menu.Types as T

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [port] <- getArgs
    startGUI defaultConfig {jsPort = Just $ read port} setup

setup :: Window -> UI ()
setup window = do
    electron <- require
    applicationMenu electron

    instructions <- UI.h1 # set text "Right click the webpage to show the menu"
    getBody window #+ [element instructions]

    return ()

applicationMenu :: JSModule "electron" -> UI ()
applicationMenu electron = do
    (evClickOpen, _, xClickOpen) <- registerEventJS
    onEvent evClickOpen $ \() -> callFunction
        (ffi "%1.remote.dialog.showOpenDialog({properties: ['openFile', 'openDirectory', 'multiSelections']})" $ electron :: JSFunction JSObject)

    (evOpenDevTools, _, xOpenDevTools) <- registerEventJS
    onEvent evOpenDevTools $ \() -> callFunction
        (ffi "%1.remote.getCurrentWindow().openDevTools()" $ electron :: JSFunction JSObject)

    (evCloseDevTools, _, xCloseDevTools) <- registerEventJS
    onEvent evCloseDevTools $ \() -> callFunction
        (ffi "%1.remote.getCurrentWindow().closeDevTools()" $ electron :: JSFunction JSObject)

    addMenu electron
        [ mkMenuItem { T.label = Just "menu1"
                     , T.submenu = Just
                       [ mkMenuItem { T.label = Just "Undo"
                                    , T.accelerator = Just $ Accelerator [CmdOrCtrl] $ Key 'Z'
                                    , T.role = Just Undo
                                    }
                       , mkMenuItem { T.label = Just "Open"
                                    , T.accelerator = Just $ Accelerator [CmdOrCtrl] $ Key 'O'
                                    , T.click = Just xClickOpen
                                    }
                       , mkMenuItem { T.label = Just "submenu1"
                                    , T.submenu = Just
                                      [ mkMenuItem { T.label = Just "item1"
                                                   , T.accelerator = Just $ Accelerator [CmdOrCtrl] $ Key 'A'
                                                   , T.click = Just xOpenDevTools
                                               }
                                      , mkMenuItem { T.label = Just "item2"
                                                   , T.accelerator = Just $ Accelerator [CmdOrCtrl] $ Key 'B'
                                                   , T.click = Just xCloseDevTools
                                                   }
                                      ]
                                    }
                       ]
                     }
        ]

    (evItem1Clicked, _, xItem1Clicked) <- registerEventJS
    onEvent evItem1Clicked $ \() -> callFunction (ffi "alert('item 1 clicked')" :: JSFunction ())
    addContextMenu electron [ mkMenuItem { T.label = Just "MenuItem1"
                                         , T.click = Just xItem1Clicked
                                         }
                            , mkMenuItem { T.label = Just "MenuItem2"
                                         , T.type_ = Just Checkbox
                                         , T.checked = Just True
                                         }
                            , mkMenuItem { T.label = Just "Disk"
                                         , T.submenu = Just
                                           [ mkMenuItem { T.type_ = Just Checkbox, T.label = Just "box1" }
                                           , mkMenuItem { T.type_ = Just Checkbox, T.label = Just "box2" }
                                           , mkMenuItem { T.type_ = Just Checkbox, T.label = Just "box3" }
                                           , mkMenuItem { T.type_ = Just Checkbox, T.label = Just "box4" }
                                           ]
                                         }
                            ]
    --TODO: Why doesn't this work more than once?
