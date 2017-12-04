{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Electron.Threepenny.Menu.Types where

import Prelude                  hiding (id)
import Data.List                (intercalate)
import Electron.Threepenny.Core
import Foreign.JavaScript       (JSObject)
import Electron.Threepenny.JSRep

data KeyModifier = Cmd | Ctrl | CmdOrCtrl | Alt | Option | AltGr | Shift | Super
instance JSRep KeyModifier where
    jsRep Cmd       = "Cmd"
    jsRep Ctrl      = "Ctrl"
    jsRep CmdOrCtrl = "CmdOrCtrl"
    jsRep Alt       = "Alt"
    jsRep Option    = "Option"
    jsRep AltGr     = "AltGr"
    jsRep Shift     = "Shift"
    jsRep Super     = "Super"

data Key = Key Char
         | Function Int
         | KeyPlus
         | KeySpace
         | KeyTab
         | KeyBackspace
         | KeyDelete
         | KeyInsert
         | KeyReturn
         | KeyUp
         | KeyDown
         | KeyLeft
         | KeyRight
         | KeyHome
         | KeyEnd
         | KeyPageUp
         | KeyPageDown
         | KeyEsc
         | KeyVolumeUp
         | KeyVolumeDown
         | KeyVolumeMute
         | KeyMediaNextTrack
         | KeyMediaPreviousTrack
         | KeyMediaStop
         | KeyMediaPlayPause
         | KeyPrintScreen
instance JSRep Key where
    jsRep (Key c)               = [c]
    jsRep (Function f)          = "F" ++ show f
    jsRep KeyPlus               = "Plus"
    jsRep KeySpace              = "Space"
    jsRep KeyTab                = "Tab"
    jsRep KeyBackspace          = "Backspace"
    jsRep KeyDelete             = "Delete"
    jsRep KeyInsert             = "Insert"
    jsRep KeyReturn             = "Return"
    jsRep KeyUp                 = "Up"
    jsRep KeyDown               = "Down"
    jsRep KeyLeft               = "Left"
    jsRep KeyRight              = "Right"
    jsRep KeyHome               = "Home"
    jsRep KeyEnd                = "End"
    jsRep KeyPageUp             = "PageUp"
    jsRep KeyPageDown           = "PageDown"
    jsRep KeyEsc                = "Esc"
    jsRep KeyVolumeUp           = "VolumeUp"
    jsRep KeyVolumeDown         = "VolumeDown"
    jsRep KeyVolumeMute         = "VolumeMute"
    jsRep KeyMediaNextTrack     = "MediaNextTrack"
    jsRep KeyMediaPreviousTrack = "MediaPreviousTrack"
    jsRep KeyMediaStop          = "MediaStop"
    jsRep KeyMediaPlayPause     = "MediaPlayPause"
    jsRep KeyPrintScreen        = "PrintScreen"

data Accelerator = Accelerator [KeyModifier] Key
instance JSRep Accelerator where
    jsRep (Accelerator m k) = "\"" ++ m' ++ "+" ++ jsRep k ++ "\""
      where
        m' = intercalate "+" $ jsRep <$> m

data Role = Undo
          | Redo
          | Cut
          | Copy
          | Paste
          | PasteAndMatchStyle
          | SelectAll
          | Delete
          | Minimize
          | Close
          | Quit
          | Reload
          | ForceReload
          | ToggleDevTools
          | ToggleFullScreen
          | ResetZoom
          | ZoomIn
          | ZoomOut
          | EditMenu
          | WindowMenu
          | About
          | Hide
          | HideOthers
          | Unhide
          | StartSpeaking
          | StopSpeaking
          | Front
          | Zoom
          | Window
          | Help
          | Services
instance JSRep Role where
    jsRep Undo               = "\"undo\""
    jsRep Redo               = "\"redo\""
    jsRep Cut                = "\"cut\""
    jsRep Copy               = "\"copy\""
    jsRep Paste              = "\"paste\""
    jsRep PasteAndMatchStyle = "\"pasteandmatchstyle\""
    jsRep SelectAll          = "\"selectall\""
    jsRep Delete             = "\"delete\""
    jsRep Minimize           = "\"minimize\""
    jsRep Close              = "\"close\""
    jsRep Quit               = "\"quit\""
    jsRep Reload             = "\"reload\""
    jsRep ForceReload        = "\"forcereload\""
    jsRep ToggleDevTools     = "\"toggledevtools\""
    jsRep ToggleFullScreen   = "\"togglefullscreen\""
    jsRep ResetZoom          = "\"resetzoom\""
    jsRep ZoomIn             = "\"zoomin\""
    jsRep ZoomOut            = "\"zoomout\""
    jsRep EditMenu           = "\"editMenu\""
    jsRep WindowMenu         = "\"windowMenu\""
    jsRep About              = "\"about\""
    jsRep Hide               = "\"hide\""
    jsRep HideOthers         = "\"hideothers\""
    jsRep Unhide             = "\"unhide\""
    jsRep StartSpeaking      = "\"startspeaking\""
    jsRep StopSpeaking       = "\"stopspeaking\""
    jsRep Front              = "\"front\""
    jsRep Zoom               = "\"zoom\""
    jsRep Window             = "\"window\""
    jsRep Help               = "\"help\""
    jsRep Services           = "\"services\""

data MenuType = Normal | Separator | Submenu | Checkbox | Radio
instance JSRep MenuType where
    jsRep Normal    = "\"normal\""
    jsRep Separator = "\"separator\""
    jsRep Submenu   = "\"submenu\""
    jsRep Checkbox  = "\"checkbox\""
    jsRep Radio     = "\"radio\""

type Menu = [MenuItem]
data MenuItem = MenuItem
    { click       :: Maybe (ExportedFunction '[()])
    , role        :: Maybe Role
    , type_       :: Maybe MenuType
    , label       :: Maybe String
    , sublabel    :: Maybe String
    , accelerator :: Maybe Accelerator
    , icon        :: Maybe String
    , enabled     :: Maybe Bool
    , visible     :: Maybe Bool
    , checked     :: Maybe Bool
    , submenu     :: Maybe Menu
    , id          :: Maybe String
    , position    :: Maybe String
    , __objs      :: Maybe [JSObject]
    }
mkMenuItem = MenuItem
    { click       = Nothing
    , role        = Nothing
    , type_       = Nothing
    , label       = Nothing
    , sublabel    = Nothing
    , accelerator = Nothing
    , icon        = Nothing
    , enabled     = Nothing
    , visible     = Nothing
    , checked     = Nothing
    , submenu     = Nothing
    , id          = Nothing
    , position    = Nothing
    , __objs      = Nothing
    }
