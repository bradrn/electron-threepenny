{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DataKinds                  #-}

module Electron.Threepenny.Menu ( buildFromTemplate
                                , addMenu
                                , addContextMenu
                                ) where

import           Control.Monad                  ((<=<))
import           Data.Maybe                     (catMaybes)
import           Data.List                      (intercalate)

import           Graphics.UI.Threepenny         ( UI
                                                , ffi
                                                , callFunction
                                                , runFunction
                                                , liftIO)
import           Foreign.JavaScript             ( JSObject
                                                , FromJS
                                                , ToJS)

import           Electron.Threepenny.Core       (JSModule)
import           Foreign.RemotePtr              (addReachable)
import           Electron.Threepenny.Core       (addReachableModule)
import qualified Electron.Threepenny.Menu.Types as T
import           Electron.Threepenny.JSRep

toTemplate :: T.MenuItem -> UI JSObject
toTemplate T.MenuItem{..} = do
    click' <- sequence $ callFunction <$> ffi "function () { %1() }" <$> click :: UI (Maybe JSObject)
    submenu' <- sequence $ (>>= combine) <$> sequence <$> (fmap toTemplate) <$> submenu :: UI (Maybe JSObject)
    let template = surround "{" "}" $ intercalate "," $
                       catMaybes [ labelObj "click"       $ "%1"                            <$  click
                                 , labelObj "role"        $ jsRep                           <$> role
                                 , labelObj "type"        $ jsRep                           <$> type_
                                 , labelObj "label"       $ jsRep                           <$> label
                                 , labelObj "sublabel"    $ jsRep                           <$> sublabel
                                 , labelObj "accelerator" $ jsRep                           <$> accelerator
                                 , labelObj "icon"        $ jsRep                           <$> icon
                                 , labelObj "enabled"     $ jsRep                           <$> enabled
                                 , labelObj "visible"     $ jsRep                           <$> visible
                                 , labelObj "checked"     $ jsRep                           <$> checked
                                 , labelObj "submenu"     $ (maybe "%1" (const "%2") click) <$  submenu
                                 , labelObj "id"          $ jsRep                           <$> id
                                 , labelObj "position"    $ jsRep                           <$> position
                                 ]
    obj <- callFunction $ ffi' template click' submenu'
    liftIO $ maybe (return ()) (addReachable obj) click'
    liftIO $ maybe (return ()) (addReachable obj) submenu'
    return obj
  where
    surround a b = (a++) . (++b)

    ffi' a (Just b) (Just c) = ffi a b c
    ffi' a (Just b) _        = ffi a b
    ffi' a _        (Just c) = ffi a   c
    ffi' a _        _        = ffi a

    labelObj s = let s' = s ++ ":" in ((s' ++) <$>)

toTemplateMenu :: T.Menu -> UI JSObject
toTemplateMenu = combine <=< sequence . fmap toTemplate

combine :: [JSObject] -> UI JSObject
combine os = do
    o <- callFunction $ ffi "%1" os
    liftIO $ traverse (addReachable o) os
    return o

newtype Menu = Menu { fromMenu :: JSObject } deriving (FromJS, ToJS)

buildFromTemplate :: JSModule "electron" -> T.Menu -> UI Menu
buildFromTemplate electron template = do
    menu <- toTemplateMenu template
    obj <- callFunction $ ffi "%1.remote.Menu.buildFromTemplate(%2)" electron menu
    liftIO $ addReachableModule electron obj
    return $ Menu obj

addMenu :: JSModule "electron" -> T.Menu -> UI ()
addMenu electron template = do
    menu <- buildFromTemplate electron template
    runFunction $ ffi "%1.remote.Menu.setApplicationMenu(%2)" electron menu

addContextMenu :: JSModule "electron" -> T.Menu -> UI ()
addContextMenu electron template = do
    menu <- buildFromTemplate electron template
    runFunction $ ffi "console.log(%1)" menu
    runFunction $ ffi "window.addEventListener('contextmenu', function (e) { e.preventDefault(); %2.popup(%1.remote.getCurrentWindow()) }, false);" electron menu

