module Main where

--import Lib
import qualified Lib2 as L
import System.Environment (getArgs)
import System.IO
import Utils.Debounce
import qualified Message as Msg


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
import qualified PhotoShake.Build as Build
import qualified PhotoShake.Id as Id
import qualified PhotoShake.Photographee2 as Photographee
import qualified PhotoShake.Control as Control

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [port, root] <- getArgs
    
    config <- toShakeConfig Nothing "config.cfg" -- Bad and unsafe

    messages <- Chan.newChan

    actionLocation <- mkDebounce defaultDebounceSettings
                 { debounceAction = writeChan messages Msg.getLocation
                 , debounceFreq = 1000000 -- 5 seconds
                 , debounceEdge = trailingEdge -- Trigger on the trailing edge
                 }

    actionDumpFiles <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getDumpFiles
             , debounceFreq = 1000000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }

    actionGetBuild <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getBuild
             , debounceFreq = 5000000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }

    actionGrades <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getGrades
             , debounceFreq = 1000000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }
    
    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
        , A.actionDumpFiles = actionDumpFiles
        , A.actionGetBuild = actionGetBuild
        , A.actionLocation = actionLocation
        , A.actionGrades= actionGrades
        , A.build = Build.noBuild
        , A.dump = noDump
        , A.dumpFiles = NoDump
        , A.dagsdato = noDagsdato
        , A.dagsdatoBackup = noDagsdato
        , A.doneshooting = noDoneshooting
        , A.photographers = noPhotographers 
        , A.photographee = Nothing
        , A.photographees = []
        , A.shootings = noShootings 
        , A.sessions = noSessions 
        , A.location = noLocation 
        , A.grades = noGrades
        , A.id = Id.noId
        , A.dir1 = "config" -- deleteme
        , A.root = fp (start root)  -- deletem
        , A.shakeConfig = config 
        , A.subscriptions = L.subscriptions
        , A.cancel = return ()
        , A.cancelDumpFiles = return ()
        , A.control = Control.Empty
        , A.cancelControl = return ()
        , A.cancelLocation = return ()
        }

    manager <- startManagerConf (WatchConfig
        { confDebounce = NoDebounce 
        , confPollInterval = 10^(6 :: Int) -- 1 second
        , confUsePolling = False
        })

    _ <- L.initialMessage messages

    L.main (read port) manager messages app 
