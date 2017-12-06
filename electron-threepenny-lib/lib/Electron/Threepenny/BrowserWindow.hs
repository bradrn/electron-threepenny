module Electron.Threepenny.BrowserWindow (module B, module D) where

{-
Here we have to manage a difficult situation. We want to export everything
EXCEPT the BrowserWindow constructor and a few utility functions. Haskell has no
'import hiding' statement, but we can accomplish this using the method from
https://mail.haskell.org/pipermail/glasgow-haskell-users/2014-October/025378.html.
However, we have to split it up into a Definitions module for almost everything
and a BrowserWindow module for the definition of the BrowserWindow newtype. This
is because we have to hide the BrowserWindow constructor, but this is only
possible in an export list and not an import list, so we need to provide a
separate export list for this newtype.
-}

import Electron.Threepenny.BrowserWindow.BrowserWindow as B
import Electron.Threepenny.BrowserWindow.Definitions as D
    hiding ( mkBrowserWindowMethodRun
           , mkBrowserWindowMethodPred
           , mkBrowserWindowMethodArg)
