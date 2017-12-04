{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Electron.Threepenny.Core
       ( JSModule
       , require
       , addReachableModule
       , ExportedFunction
       , export
       , registerEventJS
       ) where

import Graphics.UI.Threepenny.Core
import Foreign.JavaScript           ( IsHandler
                                    , JSObject
                                    , FromJS
                                    , root)
import Foreign.RemotePtr            (addReachable)
import Control.Monad                ((>=>), void)
import GHC.TypeLits                 ( KnownSymbol
                                    , Symbol
                                    , symbolVal)
import Data.Proxy                   (Proxy(..))

newtype JSModule (m :: Symbol) = JSModule JSObject deriving (ToJS)

require :: forall m. KnownSymbol m => UI (JSModule m)
require = JSModule <$> obj
  where
    obj :: UI JSObject
    obj = do
        obj <- callFunction $ ffi "require(%1)" $ symbolVal (Proxy :: Proxy m)
        rootObj <- liftJSWindow $ return . root
        liftIO $ addReachable rootObj obj
        return obj

addReachableModule :: JSModule m -> JSObject -> IO ()
addReachableModule (JSModule mod) = addReachable mod

class MkHandler h where
    type AssocHandler h
    mkHandler :: h -> Window -> AssocHandler h
instance MkHandler (UI a) where
    type AssocHandler (UI a) = IO ()
    mkHandler :: UI a -> Window -> IO ()
    mkHandler h win = void $ runUI win h
instance MkHandler a => MkHandler (b -> a) where
    type AssocHandler (b -> a) = b -> AssocHandler a
    mkHandler :: (b -> a) -> Window -> (b -> (AssocHandler a))
    mkHandler h win = \b -> mkHandler (h b) win

newtype ExportedFunction (h :: [*]) = ExportedFunction { getObj :: JSObject } deriving (FromJS, ToJS)

type family ExportedParams h :: [*] where
    ExportedParams (IO ())  = '[]
    ExportedParams (a -> b) = a : ExportedParams b

export :: (IsHandler h) => h -> UI (ExportedFunction (ExportedParams h))
export = ffiExport >=> return . ExportedFunction

registerEventJS :: (FromJS a) => UI (Event a, Handler a, ExportedFunction '[a])
registerEventJS = do
    (e, h) <- liftIO newEvent
    f <- export h
    return (e, h, f)
