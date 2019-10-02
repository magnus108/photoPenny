{-# LANGUAGE OverloadedStrings #-}

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

import Test.WebDriver.Common.Keys (enter)


import Message (block, setDump, setDoneshooting, setDagsdato, setLocation, setDagsdatoBackup)
import PhotoShake.State

import PhotoShake.ShakeConfig hiding (setDump, setDoneshooting, setDagsdato, getPhotographers, setPhotographers, setLocation, setDagsdatoBackup)
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
runSessionThenClose action = runSession chromeConfig . finallyClose $ action



setupApp :: Chan Msg.Message -> IO ()
setupApp messages = do
    config <- toShakeConfig Nothing "test/config.cfg" -- Bad and unsafe

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

    manager <- startManager
    _ <- L.initialMessage messages
    
    
    L.main 9000 manager messages app


dagsdato :: IO ()
dagsdato = do
    messages <- Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] DagsdatoBackup []

    race_ (setupApp messages)
        (runSessionThenClose $ do                      
            openPage "http://localhost:9000"

            forM_ [1..2] (\x -> do
                liftBase $ threadDelay 5000
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ Message.setDagsdatoBackup $ DA.yesDagsdato "/home/magnus/Downloads"

                liftBase $ threadDelay 5000
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 10000000 $ findElem ( ById "dagsdatoBackupPath" ) >>= getText >>= \x -> expect (x == "/home/magnus/Downloads")

                liftBase $ threadDelay 5000
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ Message.setDagsdatoBackup $ DA.noDagsdato

                liftBase $ threadDelay 5000
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 10000000 $ findElem ( ById "dagsdatoBackupMissing" )
                )
        )

main :: IO ()
main = do
    dagsdato


    messages <- Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Control []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    race_ (setupApp messages)
        (runSessionThenClose $ do                      

            openPage "http://localhost:9000"

            liftBase $ threadDelay 5000
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty


            forM_ [1..10] (\x -> do
                liftBase $ threadDelay 5000
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                liftBase $ writeChan messages $ Message.setGrades $ Grade.yesGrades $ ListZipper ["A"] "B" ["C"]

                liftBase $ threadDelay 5000
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                liftBase $ writeChan messages $ Message.setGrades $ Grade.yesGrades $ ListZipper [] "A" ["B","C"]
                )
        )

    messages <- Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Control []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
    liftBase $ writeChan messages $ setLocation $ Location.yesLocation "/home/magnus/Downloads/cis.csv"
    liftBase $ writeChan messages $ Message.setGrades $ Grade.yesGrades $ ListZipper [] "PKB" [] 
    liftBase $ writeChan messages $ setDoneshooting $ DO.yesDoneshooting "/home/magnus/Documents/projects/photoPenny/test/doneshooting/"

    race_ (setupApp messages)
        (runSessionThenClose $ do                      

            openPage "http://localhost:9000"

            liftBase $ threadDelay 5000
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty

            forM_ [1..10] (\x -> do
                liftBase $ threadDelay 5000
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                liftBase $ writeFile "/home/magnus/Documents/projects/photoPenny/test/doneshooting/cis/cr2/PKB/10.SYS_77201.1.CC.001.cr2" ""

                liftBase $ threadDelay 5000
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                waitUntil 10000 $ findElem (ById "SYS_77201")
                
                liftBase $ threadDelay 5000
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                liftBase $ removeFile "/home/magnus/Documents/projects/photoPenny/test/doneshooting/cis/cr2/PKB/10.SYS_77201.1.CC.001.cr2"

                liftBase $ threadDelay 5000
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )


    messages <- Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Main []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
    liftBase $ writeChan messages $ setDump $ D.yesDump "/home/magnus/Documents/projects/photoPenny/test/files"

    race_ (setupApp messages)
        (runSessionThenClose $ do                      

            openPage "http://localhost:9000"

            liftBase $ threadDelay 5000
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty


            forM_ [1..10] (\x -> do
                liftBase $ threadDelay 5000
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                
                files <- liftBase $ listDirectory "/home/magnus/Documents/projects/photoPenny/test/images"  
                liftBase $ mapM_ (\ f -> copyFile ("/home/magnus/Documents/projects/photoPenny/test/images" </> f) ("/home/magnus/Documents/projects/photoPenny/test/files" </> f)) files

                liftBase $ threadDelay 5000
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty


                waitUntil 10000 $ (\result -> expect (result == "32")) =<< getText =<< findElem (ById "count")

                liftBase $ threadDelay 5000
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                files <- liftBase $ listDirectory "/home/magnus/Documents/projects/photoPenny/test/files"  
                liftBase $ mapM_ (\f -> removeFile ("/home/magnus/Documents/projects/photoPenny/test/files" </> f)) files

                liftBase $ threadDelay 5000
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                )
        )



    messages <- Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Main []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    race_ (setupApp messages)
        (runSessionThenClose $ do                      

            openPage "http://localhost:9000"

            liftBase $ takeMVar empty
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty


            forM_ [1..10] (\x -> do
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                fotoId <- waitUntil 5000 $ findElem ( ById "fotoId" ) 
                sendKeys "1234" fotoId

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                liftBase $ threadDelay 5000

                liftBase $ writeChan messages $ Message.setId $ Id.noId

                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )

    messages <- Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Location []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
        
    race_ (setupApp messages)
        (runSessionThenClose $ do                      
            openPage "http://localhost:9000"
            liftBase $ takeMVar empty

            forM_ [1..5] (\x -> do
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ setLocation $ Location.yesLocation "/home/magnus/Documents/projects/photoShake/locations/naerum_skole.csv"
 
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 5000 $ findElem ( ById "locationPath" ) >>= getText >>= \x -> expect (x == "/home/magnus/Documents/projects/photoShake/locations/naerum_skole.csv")

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ setLocation $ Location.noLocation

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 5000 $ findElem ( ById "locationMissing" )
                
                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )


    messages <- Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Shooting []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    race_ (setupApp messages)
        (runSessionThenClose $ do                      

            openPage "http://localhost:9000"

            liftBase $ takeMVar empty
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty


            forM_ [1..2] (\x -> do
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                shootings <- liftBase $ A.interpret $ Shooting.getShootings $ fp $ start $ "/home/magnus/Documents/projects/photoPenny/imports/shooting.json" -- cant run on all system and this should not read a file
                liftBase $ writeChan messages $ Message.setShootings $ shootings

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 5000 $ findElem ( ById "shootingOK" ) 

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ Message.setShootings $ Shooting.noShootings

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 5000 $ findElem ( ById "shootingMissing" )

                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )

    messages <- Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Session []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    race_ (setupApp messages)
        (runSessionThenClose $ do                      

            openPage "http://localhost:9000"

            liftBase $ takeMVar empty
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty

            forM_ [1..2] (\x -> do
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                sessions <- liftBase $ A.interpret $ Session.getSessions $ fp $ start $ "/home/magnus/Documents/projects/photoPenny/imports/session.json" -- cant run on all system and this should not read a file
                liftBase $ writeChan messages $ Message.setSessions $ sessions

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 5000 $ findElem ( ById "sessionOK" ) 

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ Message.setSessions $ Session.noSessions

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 5000 $ findElem ( ById "sessionMissing" )

                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )


    messages <- Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Photographer []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    race_ (setupApp messages)
        (runSessionThenClose $ do                      

            openPage "http://localhost:9000"

            liftBase $ takeMVar empty
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty

            forM_ [1..2] (\x -> do
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                photographers <- liftBase $ A.interpret $ Photographer.getPhotographers $ fp $ start $ "/home/magnus/Documents/projects/photoPenny/imports/photographers.json" -- cant run on all system and this should not read a file
                liftBase $ writeChan messages $ Message.setPhotographers $ photographers 

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 5000 $ findElem ( ById "photographerOK" ) 

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ Message.setPhotographers $ Photographer.noPhotographers

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 5000 $ findElem ( ById "photographersMissing" )

                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )

    messages <- Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Dagsdato []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    race_ (setupApp messages)
        (runSessionThenClose $ do                      
            openPage "http://localhost:9000"

            liftBase $ takeMVar empty
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty


            forM_ [1..2] (\x -> do
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ setDagsdato $ DA.yesDagsdato "/home/magnus/Downloads"

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 5000 $ findElem ( ById "dagsdatoPath" ) >>= getText >>= \x -> expect (x == "/home/magnus/Downloads")

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ setDagsdato $ DA.noDagsdato

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 5000 $ findElem ( ById "dagsdatoMissing" )

                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )

    messages <- Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Doneshooting []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    race_ (setupApp messages)
        (runSessionThenClose $ do                      
            openPage "http://localhost:9000"

            liftBase $ takeMVar empty
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty


            forM_ [1..2] (\x -> do
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ setDoneshooting $ DO.yesDoneshooting "/home/magnus/Downloads/Magnus Renamed/what"

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 5000 $ findElem ( ById "doneshootingPath" ) >>= getText >>= \x -> expect (x == "/home/magnus/Downloads/Magnus Renamed/what")

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ setDoneshooting $ DO.noDoneshooting

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 5000 $ findElem ( ById "doneshootingMissing" )
                
                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )
        

    messages <- Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Dump []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
    
    race_ (setupApp messages)
        (runSessionThenClose $ do                      
            openPage "http://localhost:9000"

            liftBase $ takeMVar empty
            
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty

            forM_ [1..2] (\x -> do
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ setDump $ D.yesDump "/home/magnus/Downloads/Magnus Renamed/what"

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 5000 $ findElem ( ById "dumpPath" ) >>= getText >>= \x -> expect (x == "/home/magnus/Downloads/Magnus Renamed/what")

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ setDump $ D.noDump 

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 5000 $ findElem ( ById "dumpMissing" )
                
                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )

    messages <- Chan.newChan
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [Dump] Photographer []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    race_ (setupApp messages)
        (runSessionThenClose $ do                      
            openPage "http://localhost:9000"

            liftBase $ takeMVar empty
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty


            forM_ [1..10] (\x -> do
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 5000 $ findElem ( ById "tabDump" ) >>= click

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                waitUntil 5000 $ findElem ( ById "tabPhotographer" ) >>= click
                
                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
            return True
        )
