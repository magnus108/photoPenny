module Main where

--import Lib
import qualified Lib2 as L
import System.Environment (getArgs)
import System.IO


import qualified Model.E as A
import Utils.Env


import Control.Concurrent
import Utils.ListZipper
import Utils.FP
import State 

import qualified Control.Concurrent.Chan as Chan


import PhotoShake.ShakeConfig 
import PhotoShake.Dump 

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [port, root] <- getArgs
    
    config <- toShakeConfig Nothing "config.cfg" -- Bad and unsafe

    app <- newMVar $ A.app $ env A.production 
        (A.model Nothing NoDump "config" (fp $ start root) "config/state.json" config)

    messages <- Chan.newChan
    L.main (read port) messages app
