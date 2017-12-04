{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Electron.Threepenny.JSRep where

class JSRep h where
    jsRep :: h -> String

instance JSRep String where
    jsRep = show

instance JSRep Bool where
    jsRep True  = "true"
    jsRep False = "false"
