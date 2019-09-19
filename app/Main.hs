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
import PhotoShake.State 

import qualified Control.Concurrent.Chan as Chan
import System.FSNotify hiding (defaultConfig)


import PhotoShake.ShakeConfig 
import PhotoShake.Dump 
import PhotoShake.Doneshooting
import PhotoShake.Dagsdato
import PhotoShake.Photographer
import PhotoShake.Shooting
import PhotoShake.Session
import PhotoShake.Location
import PhotoShake.Location
import PhotoShake.Grade

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [port, root] <- getArgs
    
    config <- toShakeConfig Nothing "config.cfg" -- Bad and unsafe

    app <- newMVar $ A.app $ env A.production 
        (A.model Nothing noDump noDagsdato noDoneshooting noPhotographers noShootings noSessions noLocation noGrades "config" (fp $ start root) config)

    messages <- Chan.newChan
    manager <- startManager

    _ <- L.initialMessage messages
    _ <- L.subscriptions manager messages app

    L.main (read port) manager messages app 
