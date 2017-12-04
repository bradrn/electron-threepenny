module Main where

import System.Environment (getArgs)
import System.IO

import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [port] <- getArgs
    startGUI defaultConfig {jsPort = Just (read port)} setup

setup :: Window -> UI ()
setup window = do
    text <- UI.h1 # set text "Hello World!"
    getBody window #+ [element text]
    return ()
