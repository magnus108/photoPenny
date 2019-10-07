{-# LANGUAGE OverloadedStrings #-}

import Test.WebDriver.Session as WD
import Test.WebDriver hiding (setLocation)
import Control.Monad.Base
import Test.WebDriver.Commands.Wait
import Utils.FP
import qualified Control.Concurrent.Chan as Chan

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import qualified Lib as L

import System.FilePath
import System.Directory

import System.FSNotify hiding (defaultConfig)
import qualified System.FSNotify as FSN

import Test.WebDriver.Common.Keys (enter)


import PhotoShake.State

import qualified PhotoShake.ShakeConfig as Config
import PhotoShake.Photographee
import qualified PhotoShake.Build as Build


import Control.Monad

import Utils.ListZipper
import qualified Utils.Actions as A
import qualified Model.E as A
import qualified Message as Message
import Utils.Env


import Control.Concurrent

import Utils.ListZipper
import Utils.FP

import qualified PhotoShake.Dump as D
import qualified PhotoShake.Doneshooting as DO
import qualified PhotoShake.Dagsdato as DA
import qualified PhotoShake.Photographer as Photographer
import qualified PhotoShake.Session as Session
import qualified PhotoShake.Shooting as Shooting
import qualified PhotoShake.Location as Location
import qualified PhotoShake.Grade as Grade
import qualified PhotoShake.Control as Control
import qualified PhotoShake.Id as Id

import Utils.Debounce
import qualified Message as Msg
chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

runSessionThenClose :: WD a -> IO a
runSessionThenClose action = runSession chromeConfig .  closeOnException $ action



setupApp :: Chan Msg.Message -> Int -> IO ()
setupApp messages port = do
    config <- Config.toShakeConfig Nothing "test/config.cfg" -- Bad and unsafe

    actionLocation <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getLocation
             , debounceFreq = 300000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }

    actionDumpFiles <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getDumpFiles
             , debounceFreq = 300000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }

    actionGetBuild <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getBuild
             , debounceFreq = 300000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }

    actionGrades <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getGrades
             , debounceFreq = 300000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }

    actionGrades2 <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getGrades
             , debounceFreq = 300000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }
    
    actionId <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getId
             , debounceFreq = 300000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }

    actionLocation <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getLocation
             , debounceFreq = 300000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }
    actionSession <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getSessions
             , debounceFreq = 300000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }
    actionShooting <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getShootings
             , debounceFreq = 300000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }
    actionPhotographer <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getPhotographers
             , debounceFreq = 300000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }
             
    actionDagsdato <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getDagsdato
             , debounceFreq = 300000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }
    actionDagsdatoBackup <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getDagsdatoBackup
             , debounceFreq = 300000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }
             
    actionDoneshooting <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getDoneshooting
             , debounceFreq = 300000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }

    actionDump <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getDump
             , debounceFreq = 300000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }

    actionState <- mkDebounce defaultDebounceSettings
             { debounceAction = writeChan messages Msg.getStates
             , debounceFreq = 300000 -- 5 seconds
             , debounceEdge = trailingEdge -- Trigger on the trailing edge
             }


    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
        , A.dumpFiles = Config.NoDump
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
        , A.dump = D.noDump
        , A.id = Id.noId
        , A.dagsdato = DA.noDagsdato
        , A.dagsdatoBackup = DA.noDagsdato
        , A.doneshooting = DO.noDoneshooting
        , A.photographers = Photographer.noPhotographers 
        , A.photographee = Nothing
        , A.photographees = []
        , A.shootings = Shooting.noShootings 
        , A.sessions = Session.noSessions 
        , A.location = Location.noLocation 
        , A.grades = Grade.noGrades
        , A.dir1 = "test/config" -- deleteme
        , A.root = fp (start "")  -- deletem
        , A.shakeConfig = config 
        , A.subscriptions = L.subscriptions
        , A.control = Control.Empty
        , A.cancel = return ()
        , A.cancelLocation = return ()
        , A.cancelDumpFiles = return ()
        , A.cancelControl = return ()
        }

    manager <- FSN.startManagerConf $ FSN.defaultConfig { FSN.confUsePolling = True, FSN.confDebounce = FSN.NoDebounce, FSN.confPollInterval = 300000 }
    _ <- L.initialMessage messages
    
    
    L.main port manager messages app


setDagsdatoBackup :: WD ()
setDagsdatoBackup = do
    messages <- liftBase $ Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] DagsdatoBackup []
    liftBase $ forkIO $ setupApp messages 9000
    liftBase $ threadDelay 5000000

    openPage "http://localhost:9000"

    forM_ [1..5] (\x -> do
        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        liftBase $ writeChan messages $ Message.setDagsdatoBackup $ DA.yesDagsdato "test/dump"

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 10000000 $ findElem ( ById "dagsdatoBackupPath" ) >>= getText >>= \x -> expect (x == "test/dump")

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        liftBase $ writeChan messages $ Message.setDagsdatoBackup $ DA.noDagsdato

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 10000000 $ findElem ( ById "dagsdatoBackupMissing" )
        )


setGrades :: WD ()
setGrades = do
    messages <- liftBase $ Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Control []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    liftBase $ forkIO $ setupApp messages 9001 

    liftBase $ threadDelay 5000000
    openPage "http://localhost:9001"

    liftBase $ threadDelay 1000000
    liftBase $ writeChan messages (Message.block empty)
    liftBase $ takeMVar empty


    forM_ [1..10] (\x -> do
        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty

        liftBase $ writeChan messages $ Message.setGrades $ Grade.yesGrades $ ListZipper ["A"] "B" ["C"]

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty

        liftBase $ writeChan messages $ Message.setGrades $ Grade.yesGrades $ ListZipper [] "A" ["B","C"]
        )

controlXMP :: WD ()
controlXMP = do
    messages <- liftBase $ Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Control []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
    liftBase $ writeChan messages $ Message.setLocation $ Location.yesLocation "test/cis.csv"
    liftBase $ writeChan messages $ Message.setGrades $ Grade.yesGrades $ ListZipper [] "PKB" [] 
    liftBase $ writeChan messages $ Message.setDoneshooting $ DO.yesDoneshooting "test/doneshooting"

    liftBase $ forkIO $ setupApp messages 9002 

    liftBase $ threadDelay 5000000
    openPage "http://localhost:9002"

    liftBase $ threadDelay 1000000
    liftBase $ writeChan messages (Message.block empty)
    liftBase $ takeMVar empty

    forM_ [1..10] (\x -> do
        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty

        liftBase $ writeFile "test/doneshooting/cis/cr2/PKB/10.SYS_77201.1.CC.001.cr2" ""

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty

        waitUntil 100000 $ findElem (ById "SYS_77201")
        
        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty

        liftBase $ removeFile "test/doneshooting/cis/cr2/PKB/10.SYS_77201.1.CC.001.cr2"

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        )

counter :: WD ()
counter = do
    messages <- liftBase $ Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Main []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
    liftBase $ writeChan messages $ Message.setDump $ D.yesDump "test/dump"

    liftBase $ forkIO $ setupApp messages 9003
    liftBase $ threadDelay 5000000

    openPage "http://localhost:9003"

    liftBase $ threadDelay 3000000
    liftBase $ writeChan messages (Message.block empty)
    liftBase $ takeMVar empty


    forM_ [1..10] (\x -> do
        liftBase $ threadDelay 3000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        
        files <- liftBase $ listDirectory "test/images"  
        liftBase $ mapM_ (\ f -> copyFile ("test/images" </> f) ("test/dump" </> f)) files

        liftBase $ threadDelay 5000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 10000 $ (\result -> expect (result == "32")) =<< getText =<< findElem (ById "count")

        liftBase $ threadDelay 3000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty

        files <- liftBase $ listDirectory "test/dump"  
        liftBase $ mapM_ (\f -> removeFile ("test/dump" </> f)) files

        liftBase $ threadDelay 3000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty

        )

setPhotoId :: WD ()
setPhotoId = do
    messages <- liftBase $ Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Main []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    liftBase $ forkIO $ setupApp messages 9004

    liftBase $ threadDelay 5000000
    openPage "http://localhost:9004"

    liftBase $ threadDelay 1000000
    liftBase $ writeChan messages (Message.block empty)
    liftBase $ takeMVar empty


    forM_ [1..10] (\x -> do
        liftBase $ threadDelay 2000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty

        fotoId <- waitUntil 50000 $ findElem ( ById "fotoId" ) 
        sendKeys "1234" fotoId

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty


        liftBase $ writeChan messages $ Message.setId $ Id.noId

        --finisher
        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        )



setLocation :: WD ()
setLocation = do
    messages <- liftBase $ Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Location []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
        
    liftBase $ forkIO $ setupApp messages 9005
    liftBase $ threadDelay 5000000
    openPage "http://localhost:9005"
    liftBase $ threadDelay 1000000
    liftBase $ writeChan messages (Message.block empty)
    liftBase $ takeMVar empty

    forM_ [1..5] (\x -> do
        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        liftBase $ writeChan messages $ Message.setLocation $ Location.yesLocation "test/cis.csv"

        liftBase $ threadDelay 2000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 50000 $ findElem ( ById "locationPath" ) >>= getText >>= \x -> expect (x == "test/cis.csv")

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        liftBase $ writeChan messages $ Message.setLocation $ Location.noLocation

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 50000 $ findElem ( ById "locationMissing" )
        
        --finisher
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        )


setShooting :: WD ()
setShooting = do
    messages <- liftBase $ Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Shooting []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    liftBase $ forkIO $ setupApp messages 9006

    liftBase $ threadDelay 5000000
    openPage "http://localhost:9006"

    liftBase $ threadDelay 1000000
    liftBase $ writeChan messages (Message.block empty)
    liftBase $ takeMVar empty


    forM_ [1..2] (\x -> do
        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty

        shootings <- liftBase $ A.interpret $ Shooting.getShootings $ fp $ start $ "imports/shooting.json" -- cant run on all system and this should not read a file
        liftBase $ writeChan messages $ Message.setShootings $ shootings

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 50000 $ findElem ( ById "shootingOK" ) 

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        liftBase $ writeChan messages $ Message.setShootings $ Shooting.noShootings

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 50000 $ findElem ( ById "shootingMissing" )

        --finisher
        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        )


setSession :: WD ()
setSession = do
    messages <- liftBase $ Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Session []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    liftBase $ forkIO $ setupApp messages 9007

    liftBase $ threadDelay 5000000
    openPage "http://localhost:9007"

    liftBase $ threadDelay 1000000
    liftBase $ writeChan messages (Message.block empty)
    liftBase $ takeMVar empty

    forM_ [1..2] (\x -> do
        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty

        sessions <- liftBase $ A.interpret $ Session.getSessions $ fp $ start $ "imports/session.json" -- cant run on all system and this should not read a file
        liftBase $ writeChan messages $ Message.setSessions $ sessions

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 5000 $ findElem ( ById "sessionOK" ) 

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        liftBase $ writeChan messages $ Message.setSessions $ Session.noSessions

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 5000$ findElem ( ById "sessionMissing" )

        --finisher
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        )


setPhotographers :: WD ()
setPhotographers = do
    messages <- liftBase $ Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Photographer []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    liftBase $ forkIO $ setupApp messages 9008

    liftBase $ threadDelay 5000000
    openPage "http://localhost:9008"

    liftBase $ threadDelay 1000000
    liftBase $ writeChan messages (Message.block empty)
    liftBase $ takeMVar empty

    forM_ [1..2] (\x -> do
        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty

        photographers <- liftBase $ A.interpret $ Photographer.getPhotographers $ fp $ start $ "imports/photographers.json" -- cant run on all system and this should not read a file
        liftBase $ writeChan messages $ Message.setPhotographers $ photographers 

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 5000 $ findElem ( ById "photographerOK" ) 

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        liftBase $ writeChan messages $ Message.setPhotographers $ Photographer.noPhotographers

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 5000 $ findElem ( ById "photographersMissing" )

        --finisher
        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        )

setDagsdato :: WD ()
setDagsdato = do
    messages <- liftBase $ Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Dagsdato []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    liftBase $ forkIO $ setupApp messages 9009
    liftBase $ threadDelay 5000000
    openPage "http://localhost:9009"

    liftBase $ threadDelay 1000000
    liftBase $ writeChan messages (Message.block empty)
    liftBase $ takeMVar empty

    forM_ [1..2] (\x -> do
        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        liftBase $ writeChan messages $ Message.setDagsdato $ DA.yesDagsdato "test/dagsdato"

        liftBase $ threadDelay 2000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 5000 $ findElem ( ById "dagsdatoPath" ) >>= getText >>= \x -> expect (x == "test/dagsdato")

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        liftBase $ writeChan messages $ Message.setDagsdato $ DA.noDagsdato

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 5000 $ findElem ( ById "dagsdatoMissing" )

        --finisher
        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        )


setDoneshooting :: WD ()
setDoneshooting = do
    messages <- liftBase $ Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Doneshooting []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    session <- WD.getSession 
    liftBase $ forkIO $ (setupApp messages 9010)
    liftBase $ threadDelay 5000000
    openPage "http://localhost:9010"

    liftBase $ threadDelay 2000000
    liftBase $ writeChan messages (Message.block empty)
    liftBase $ takeMVar empty

    forM_ [1..2] (\x -> do
        liftBase $ threadDelay 2000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        liftBase $ writeChan messages $ Message.setDoneshooting $ DO.yesDoneshooting "test/doneshooting"

        liftBase $ threadDelay 2000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 5000 $ findElem ( ById "doneshootingPath" ) >>= getText >>= \x -> expect (x == "test/doneshooting")

        liftBase $ threadDelay 2000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        liftBase $ writeChan messages $ Message.setDoneshooting $ DO.noDoneshooting

        liftBase $ threadDelay 2000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 5000 $ findElem ( ById "doneshootingMissing" )
        
        --finisher
        liftBase $ threadDelay 2000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        )
            

setDump :: WD ()
setDump = do
    messages <- liftBase $ Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Dump []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
    
    liftBase $ forkIO $ setupApp messages 9011
    liftBase $ threadDelay 500000
    openPage "http://localhost:9011"
    
    liftBase $ threadDelay 1000000
    liftBase $ writeChan messages (Message.block empty)
    liftBase $ takeMVar empty

    forM_ [1..2] (\x -> do
        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        liftBase $ writeChan messages $ Message.setDump $ D.yesDump "test/dump"

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 5000 $ findElem ( ById "dumpPath" ) >>= getText >>= \x -> expect (x == "test/dump")

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        liftBase $ writeChan messages $ Message.setDump $ D.noDump 

        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        waitUntil 5000 $ findElem ( ById "dumpMissing" )
        
        --finisher
        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        )

setState :: WD ()
setState = do
    messages <- liftBase $ Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [Dump] Photographer []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    liftBase $ forkIO $ setupApp messages 9012
            
    liftBase $ threadDelay 5000000
    openPage "http://localhost:9012"

    liftBase $ threadDelay 1000000
    liftBase $ writeChan messages (Message.block empty)
    liftBase $ takeMVar empty


    forM_ [1..10] (\x -> do
        liftBase $ threadDelay 5000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty

        waitUntil 50000 $ findElem ( ById "tabDump" ) >>= click

        liftBase $ threadDelay 5000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty

        waitUntil 50000 $ findElem ( ById "tabPhotographer" ) >>= click
        
        --finisher
        liftBase $ threadDelay 1000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty
        )



setGradeDropDown :: WD ()
setGradeDropDown = do
    messages <- liftBase $ Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Control []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
    liftBase $ writeChan messages $ Message.setGrades $ Grade.yesGrades $ ListZipper ["stue"] "ggg" ["ccc"]

    liftBase $ forkIO $ setupApp messages 9013
    liftBase $ threadDelay 5000000
    openPage "http://localhost:9013"

    liftBase $ threadDelay 1000000
    liftBase $ writeChan messages (Message.block empty)
    liftBase $ takeMVar empty


    forM_ [1..10] (\x -> do
        liftBase $ threadDelay 5000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty

        waitUntil 50000 $ findElem ( ById "inputter" ) >>= click
        liftBase $ threadDelay 1000000

        waitUntil 50000 $ findElem ( ById "stue" ) >>= click
        
        --finisher
        liftBase $ threadDelay 5000000
        liftBase $ writeChan messages (Message.block empty)
        liftBase $ takeMVar empty

        waitUntil 50000 $ findElem ( ById "inputter" ) >>= click

        waitUntil 50000 $ findElem ( ById "ggg" ) >>= click 
        )






main :: IO ()
main = do
    runSessionThenClose $ do
        setDagsdatoBackup 
        liftBase $ putStrLn "11"
        setDoneshooting
        liftBase $ putStrLn "12"
        setDump
        liftBase $ putStrLn "2"
        counter
        liftBase $ putStrLn "5"
        setPhotoId
        liftBase $ putStrLn "6"
        setLocation
        liftBase $ putStrLn "7"
        setShooting
        liftBase $ putStrLn "8"
        setSession
        liftBase $ putStrLn "9"
        setPhotographers
        liftBase $ putStrLn "10"
        setDagsdato
        liftBase $ putStrLn "13"
        setState
        liftBase $ putStrLn "14"
        setGradeDropDown 
        liftBase $ putStrLn "15"
        controlXMP 
        liftBase $ putStrLn "BADNESS"
        setGrades
        return ()
