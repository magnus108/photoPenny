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
    
    --- FIX ME PLS
    --- FIX ME PLS
    --- FIX ME PLS
    config <- toShakeConfig (Just root) "config.cfg" -- Bad and unsafe

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

    actionGrades2 <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getGrades
             , debounceFreq = 1000000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }
    
    actionId <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getId
             , debounceFreq = 1000000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }

    actionLocation <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getLocation
             , debounceFreq = 1000000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }
    actionSession <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getSessions
             , debounceFreq = 1000000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }
    actionShooting <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getShootings
             , debounceFreq = 1000000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }
    actionPhotographer <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getPhotographers
             , debounceFreq = 1000000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }
             
    actionDagsdato <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getDagsdato
             , debounceFreq = 1000000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }
    actionDagsdatoBackup <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getDagsdatoBackup
             , debounceFreq = 1000000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }
             
    actionDoneshooting <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getDoneshooting
             , debounceFreq = 1000000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }

    actionDump <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getDump
             , debounceFreq = 1000000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }

    actionState <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getStates
             , debounceFreq = 1000000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }
             
    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
        , A.actionSession = actionSession
        , A.actionShooting = actionShooting
        , A.actionPhotographer = actionPhotographer
        , A.actionDagsdato = actionDagsdato
        , A.actionDagsdatoBackup = actionDagsdatoBackup
        , A.actionState = actionState
        , A.actionDump = actionDump
        , A.actionDoneshooting = actionDoneshooting
        

        , A.actionLocation = actionLocation 
        , A.actionDumpFiles = actionDumpFiles
        , A.actionGetBuild = actionGetBuild
        , A.actionGrades = actionGrades
        , A.actionGrades2 = actionGrades2
        , A.actionId = actionId
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

    manager <- startManagerConf defaultConfig
    _ <- L.initialMessage messages

    L.main (read port) manager messages app 
