module Main where

import Lib
import System.Environment (getArgs)
import System.IO


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [port, root] <- getArgs
    someFunc (read port) root

