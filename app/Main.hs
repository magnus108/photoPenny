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
import System.FSNotify 


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
import qualified PhotoShake.Control as Control

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [port, root] <- getArgs
    
    config <- toShakeConfig Nothing "config.cfg" -- Bad and unsafe

    
    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
        , A.dump = noDump
        , A.dagsdato = noDagsdato
        , A.doneshooting = noDoneshooting
        , A.photographers = noPhotographers 
        , A.shootings = noShootings 
        , A.sessions = noSessions 
        , A.location = noLocation 
        , A.grades = noGrades
        , A.dir1 = "config" -- deleteme
        , A.root = fp (start root)  -- deletem
        , A.shakeConfig = config 
        , A.subscriptions = L.subscriptions
        , A.cancel = return ()
        , A.control = Control.Empty
        }

    messages <- Chan.newChan
    manager <- startManagerConf (defaultConfig { confDebounce = Debounce 0.1 } )

    _ <- L.initialMessage messages

    L.main (read port) manager messages app 
