{-# LANGUAGE OverloadedStrings #-}

import Test.WebDriver hiding (setLocation)
import Control.Monad.Base
import Test.WebDriver.Commands.Wait
import Utils.FP
import qualified Control.Concurrent.Chan as Chan

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import qualified Lib2 as L

import System.FilePath
import System.Directory

import System.FSNotify hiding (defaultConfig)

import Test.WebDriver.Common.Keys (enter)


import Message (block, setDump, setDoneshooting, setDagsdato, setLocation, setDagsdatoBackup)
import PhotoShake.State

import PhotoShake.ShakeConfig hiding (setDump, setDoneshooting, setDagsdato, getPhotographers, setPhotographers, setLocation, setDagsdatoBackup)
import PhotoShake.Photographee
import PhotoShake.Built


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

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

runSessionThenClose :: WD a -> IO a
runSessionThenClose action = runSession chromeConfig . finallyClose $ action

main :: IO ()
main = do
    config <- toShakeConfig Nothing "test/config.cfg" -- Bad and unsafe

    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
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
        , A.cancelDumpFiles = return ()
        , A.cancelControl = return ()
        }

    messages <- Chan.newChan
    manager <- startManager
    _ <- L.initialMessage messages
    
    --fuckthis
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] DagsdatoBackup []
    --fuckthis
    
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages (block empty)


    race_ (L.main 9000 manager messages app)
        (runSessionThenClose $ do                      
            openPage "http://localhost:9000"

            forM_ [1..2] (\x -> do
                liftBase $ threadDelay 1000001
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ Message.setDagsdatoBackup $ DA.yesDagsdato "/home/magnus/Downloads"

                liftBase $ threadDelay 1000001
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 10000000 $ findElem ( ById "dagsdatoBackupPath" ) >>= getText >>= \x -> expect (x == "/home/magnus/Downloads")

                liftBase $ threadDelay 1000001
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ Message.setDagsdatoBackup $ DA.noDagsdato

                liftBase $ threadDelay 1000001
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 10000000 $ findElem ( ById "dagsdatoBackupMissing" )
                )
        )

    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
        , A.dump = D.noDump
        , A.dagsdato = DA.noDagsdato
        , A.dagsdatoBackup = DA.noDagsdato
        , A.doneshooting = DO.noDoneshooting
        , A.photographers = Photographer.noPhotographers 
        , A.photographee = Nothing
        , A.photographees = []
        , A.shootings = Shooting.noShootings 
        , A.id = Id.noId
        , A.sessions = Session.noSessions 
        , A.location = Location.noLocation 
        , A.grades = Grade.noGrades
        , A.dir1 = "test/config" -- deleteme
        , A.root = fp (start "")  -- deletem
        , A.shakeConfig = config 
        , A.subscriptions = L.subscriptions
        , A.control = Control.Empty
        , A.cancel = return ()
        , A.cancelDumpFiles = return ()
        , A.cancelControl = return ()
        }

    messages <- Chan.newChan
    manager <- startManager
    _ <- L.initialMessage messages

    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Control []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    empty <- liftBase newEmptyMVar

    -- WAIT filepooling TIMEOUT..
    -- WAIT filepooling TIMEOUT..
    -- WAIT filepooling TIMEOUT..

    race_ (L.main 9000 manager messages app)
        (runSessionThenClose $ do                      

            openPage "http://localhost:9000"

            liftBase $ threadDelay 1000001
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty


            forM_ [1..10] (\x -> do
                liftBase $ threadDelay 1000001
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                liftBase $ writeChan messages $ Message.setGrades $ Grade.yesGrades $ ListZipper ["A"] "B" ["C"]

                liftBase $ threadDelay 1000001
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                liftBase $ writeChan messages $ Message.setGrades $ Grade.yesGrades $ ListZipper [] "A" ["B","C"]
                )
        )

    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
        , A.dump = D.noDump
        , A.dagsdato = DA.noDagsdato
        , A.dagsdatoBackup = DA.noDagsdato
        , A.doneshooting = DO.noDoneshooting
        , A.photographers = Photographer.noPhotographers 
        , A.photographee = Nothing
        , A.photographees = []
        , A.shootings = Shooting.noShootings 
        , A.id = Id.noId
        , A.sessions = Session.noSessions 
        , A.location = Location.noLocation 
        , A.grades = Grade.noGrades
        , A.dir1 = "test/config" -- deleteme
        , A.root = fp (start "")  -- deletem
        , A.shakeConfig = config 
        , A.subscriptions = L.subscriptions
        , A.control = Control.Empty
        , A.cancel = return ()
        , A.cancelDumpFiles = return ()
        , A.cancelControl = return ()
        }

    messages <- Chan.newChan
    manager <- startManager
    _ <- L.initialMessage messages

    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Control []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
    liftBase $ writeChan messages $ setLocation $ Location.yesLocation "/home/magnus/Downloads/cis.csv"
    liftBase $ writeChan messages $ Message.setGrades $ Grade.yesGrades $ ListZipper [] "PKB" [] 
    liftBase $ writeChan messages $ setDoneshooting $ DO.yesDoneshooting "/home/magnus/Documents/projects/photoPenny/test/doneshooting/"
    empty <- liftBase newEmptyMVar

    -- WAIT filepooling TIMEOUT..
    -- WAIT filepooling TIMEOUT..
    -- WAIT filepooling TIMEOUT..

    race_ (L.main 9000 manager messages app)
        (runSessionThenClose $ do                      

            openPage "http://localhost:9000"

            liftBase $ threadDelay 1000001
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty


            forM_ [1..10] (\x -> do
                liftBase $ threadDelay 1000001
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                liftBase $ writeFile "/home/magnus/Documents/projects/photoPenny/test/doneshooting/cis/cr2/PKB/10.SYS_77201.1.CC.001.cr2" ""

                liftBase $ threadDelay 1000001
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                waitUntil 10000 $ findElem (ById "SYS_77201")
                
                liftBase $ threadDelay 1000001
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                liftBase $ removeFile "/home/magnus/Documents/projects/photoPenny/test/doneshooting/cis/cr2/PKB/10.SYS_77201.1.CC.001.cr2"

                liftBase $ threadDelay 1000001
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )



    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
        , A.dump = D.noDump
        , A.dagsdato = DA.noDagsdato
        , A.dagsdatoBackup = DA.noDagsdato
        , A.doneshooting = DO.noDoneshooting
        , A.photographers = Photographer.noPhotographers 
        , A.photographee = Nothing
        , A.photographees = []
        , A.shootings = Shooting.noShootings 
        , A.id = Id.noId
        , A.sessions = Session.noSessions 
        , A.location = Location.noLocation 
        , A.grades = Grade.noGrades
        , A.dir1 = "test/config" -- deleteme
        , A.root = fp (start "")  -- deletem
        , A.shakeConfig = config 
        , A.subscriptions = L.subscriptions
        , A.control = Control.Empty
        , A.cancel = return ()
        , A.cancelDumpFiles = return ()
        , A.cancelControl = return ()
        }

    messages <- Chan.newChan
    manager <- startManager
    _ <- L.initialMessage messages

    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Main []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
    liftBase $ writeChan messages $ setDump $ D.yesDump "/home/magnus/Documents/projects/photoPenny/test/files"

    empty <- liftBase newEmptyMVar

    -- WAIT filepooling TIMEOUT..
    -- WAIT filepooling TIMEOUT..
    -- WAIT filepooling TIMEOUT..

    race_ (L.main 9000 manager messages app)
        (runSessionThenClose $ do                      

            openPage "http://localhost:9000"

            liftBase $ threadDelay 1000001
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty


            forM_ [1..10] (\x -> do
                liftBase $ threadDelay 1000001
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                
                files <- liftBase $ listDirectory "/home/magnus/Documents/projects/photoPenny/test/images"  
                liftBase $ mapM_ (\ f -> copyFile ("/home/magnus/Documents/projects/photoPenny/test/images" </> f) ("/home/magnus/Documents/projects/photoPenny/test/files" </> f)) files

                liftBase $ threadDelay 1000001
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty


                waitUntil 10000 $ (\result -> expect (result == "32")) =<< getText =<< findElem (ById "count")

                liftBase $ threadDelay 1000001
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                files <- liftBase $ listDirectory "/home/magnus/Documents/projects/photoPenny/test/files"  
                liftBase $ mapM_ (\f -> removeFile ("/home/magnus/Documents/projects/photoPenny/test/files" </> f)) files

                liftBase $ threadDelay 1000001
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty


                waitUntil 10000 $ (\result -> expect (result == "0")) =<< getText =<< findElem (ById "count")
                )
        )



    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
        , A.dump = D.noDump
        , A.dagsdato = DA.noDagsdato
        , A.dagsdatoBackup = DA.noDagsdato
        , A.doneshooting = DO.noDoneshooting
        , A.photographers = Photographer.noPhotographers 
        , A.photographee = Nothing
        , A.photographees = []
        , A.shootings = Shooting.noShootings 
        , A.id = Id.noId
        , A.sessions = Session.noSessions 
        , A.location = Location.noLocation 
        , A.grades = Grade.noGrades
        , A.dir1 = "test/config" -- deleteme
        , A.root = fp (start "")  -- deletem
        , A.shakeConfig = config 
        , A.subscriptions = L.subscriptions
        , A.control = Control.Empty
        , A.cancel = return ()
        , A.cancelDumpFiles = return ()
        , A.cancelControl = return ()
        }

    messages <- Chan.newChan
    manager <- startManager
    _ <- L.initialMessage messages

    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Main []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages (block empty)

    race_ (L.main 9000 manager messages app)
        (runSessionThenClose $ do                      

            openPage "http://localhost:9000"

            liftBase $ takeMVar empty
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty


            forM_ [1..10] (\x -> do
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                fotoId <- waitUntil 10000000 $ findElem ( ById "fotoId" ) 
                sendKeys "1234" fotoId

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                liftBase $ threadDelay 1000000

                liftBase $ writeChan messages $ Message.setId $ Id.noId

                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )

    -- dangerous difference between these params

    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
        , A.dump = D.noDump
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
        , A.id = Id.noId
        , A.dir1 = "test/config" -- deleteme
        , A.root = fp (start "")  -- deletem
        , A.shakeConfig = config 
        , A.subscriptions = L.subscriptions
        , A.control = Control.Empty
        , A.cancel = return ()
        , A.cancelDumpFiles = return ()
        , A.cancelControl = return ()
        }

    messages <- Chan.newChan
    manager <- startManager
    _ <- L.initialMessage messages


    --fuckthis
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Location []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
    --fuckthis
    
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages (block empty)
        
    race_ (L.main 9000 manager messages app)
        (runSessionThenClose $ do                      
            openPage "http://localhost:9000"
            liftBase $ takeMVar empty

            forM_ [1..5] (\x -> do
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ setLocation $ Location.yesLocation "/home/magnus/Documents/projects/photoShake/locations/naerum_skole.csv"
 
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 10000000 $ findElem ( ById "locationPath" ) >>= getText >>= \x -> expect (x == "/home/magnus/Documents/projects/photoShake/locations/naerum_skole.csv")

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ setLocation $ Location.noLocation

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 10000000 $ findElem ( ById "locationMissing" )
                
                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )


    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
        , A.dump = D.noDump
        , A.dagsdato = DA.noDagsdato
        , A.dagsdatoBackup = DA.noDagsdato
        , A.doneshooting = DO.noDoneshooting
        , A.photographers = Photographer.noPhotographers 
        , A.photographee = Nothing
        , A.photographees = []
        , A.shootings = Shooting.noShootings 
        , A.id = Id.noId
        , A.sessions = Session.noSessions 
        , A.location = Location.noLocation 
        , A.grades = Grade.noGrades
        , A.dir1 = "test/config" -- deleteme
        , A.root = fp (start "")  -- deletem
        , A.shakeConfig = config 
        , A.subscriptions = L.subscriptions
        , A.control = Control.Empty
        , A.cancel = return ()
        , A.cancelDumpFiles = return ()
        , A.cancelControl = return ()
        }

    messages <- Chan.newChan
    manager <- startManager
    _ <- L.initialMessage messages

    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Shooting []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages (block empty)

    race_ (L.main 9000 manager messages app)
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
                waitUntil 10000000 $ findElem ( ById "shootingOK" ) 

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ Message.setShootings $ Shooting.noShootings

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 10000000 $ findElem ( ById "shootingMissing" )

                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )


    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
        , A.dump = D.noDump
        , A.dagsdato = DA.noDagsdato
        , A.dagsdatoBackup = DA.noDagsdato
        , A.doneshooting = DO.noDoneshooting
        , A.photographers = Photographer.noPhotographers 
        , A.photographee = Nothing
        , A.photographees = []
        , A.id = Id.noId
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
        , A.cancelDumpFiles = return ()
        , A.cancelControl = return ()
        }

    messages <- Chan.newChan
    manager <- startManager
    _ <- L.initialMessage messages
    
    --fuckthis
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Session []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
    --fuckthis
    
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages (block empty)

    race_ (L.main 9000 manager messages app)
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
                waitUntil 10000000 $ findElem ( ById "sessionOK" ) 

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ Message.setSessions $ Session.noSessions

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 10000000 $ findElem ( ById "sessionMissing" )

                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )


    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
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
        , A.cancelDumpFiles = return ()
        , A.cancelControl = return ()
        }

    messages <- Chan.newChan
    manager <- startManager
    _ <- L.initialMessage messages
            
    --fuckthis
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Photographer []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
    --fuckthis
            
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages (block empty)

    race_ (L.main 9000 manager messages app)
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
                waitUntil 10000000 $ findElem ( ById "photographerOK" ) 

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ Message.setPhotographers $ Photographer.noPhotographers

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 10000000 $ findElem ( ById "photographersMissing" )

                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )

    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
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
        , A.cancelDumpFiles = return ()
        , A.cancelControl = return ()
        }

    messages <- Chan.newChan
    manager <- startManager
    _ <- L.initialMessage messages
    
    --fuckthis
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Dagsdato []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
    --fuckthis
    
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages (block empty)


    race_ (L.main 9000 manager messages app)
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
                waitUntil 10000000 $ findElem ( ById "dagsdatoPath" ) >>= getText >>= \x -> expect (x == "/home/magnus/Downloads")

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ setDagsdato $ DA.noDagsdato

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 10000000 $ findElem ( ById "dagsdatoMissing" )

                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )

    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
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
        , A.cancelDumpFiles = return ()
        , A.cancelControl = return ()
        }


    messages <- Chan.newChan
    manager <- startManager
    _ <- L.initialMessage messages
            
    --fuckthis
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Doneshooting []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
    --fuckthis
            
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages (block empty)

    race_ (L.main 9000 manager messages app)
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
                waitUntil 10000000 $ findElem ( ById "doneshootingPath" ) >>= getText >>= \x -> expect (x == "/home/magnus/Downloads/Magnus Renamed/what")

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ setDoneshooting $ DO.noDoneshooting

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 10000000 $ findElem ( ById "doneshootingMissing" )
                
                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )

    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
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
        , A.cancelDumpFiles = return ()
        , A.cancelControl = return ()
        }

    messages <- Chan.newChan
    manager <- startManager
    _ <- L.initialMessage messages
    --fuckthis
    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [] Dump []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []
    --fuckthis
    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages (block empty)
    
    race_ (L.main 9000 manager messages app)
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
                waitUntil 10000000 $ findElem ( ById "dumpPath" ) >>= getText >>= \x -> expect (x == "/home/magnus/Downloads/Magnus Renamed/what")

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                liftBase $ writeChan messages $ setDump $ D.noDump 

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 10000000 $ findElem ( ById "dumpMissing" )
                
                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
        )

    app <- newMVar $ A.app $ env A.production $ A.Model
        { A.states = Nothing
        , A.dump = D.noDump
        , A.dagsdato = DA.noDagsdato
        , A.dagsdatoBackup = DA.noDagsdato
        , A.id = Id.noId
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
        , A.cancelDumpFiles = return ()
        , A.cancelControl = return ()
        }

    messages <- Chan.newChan
    manager <- startManager
    _ <- L.initialMessage messages

    liftBase $ writeChan messages $ Message.setStates $ States $ ListZipper [Dump] Photographer []
    liftBase $ writeChan messages $ Message.setSessions $ Session.yesSessions $ ListZipper [] Session.school []

    empty <- liftBase newEmptyMVar
    liftBase $ writeChan messages (block empty)

    race_ (L.main 9000 manager messages app)
        (runSessionThenClose $ do                      
            openPage "http://localhost:9000"

            liftBase $ takeMVar empty
            liftBase $ writeChan messages (block empty)
            liftBase $ takeMVar empty


            forM_ [1..10] (\x -> do
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                waitUntil 100000000 $ findElem ( ById "tabDump" ) >>= click

                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty

                waitUntil 100000000 $ findElem ( ById "tabPhotographer" ) >>= click
                
                --finisher
                liftBase $ writeChan messages (block empty)
                liftBase $ takeMVar empty
                )
            return True
        )




            --fmap and $  forM [1..40] (\iter -> do
                    --_ <- liftBase $ putMVar app' app''
{-
                    app'' <- liftBase $ takeMVar app'
                    empty <- liftBase newEmptyMVar
                    liftBase $ writeChan messages (block empty)
                    liftBase $ readMVar empty
                    waitUntil 10000000 $ findElem ( ById "tabDoneshooting" ) >>= click
                    _ <- liftBase $ putMVar app' app''
                    app'' <- liftBase $ takeMVar app'
                    empty <- liftBase newEmptyMVar
                    liftBase $ writeChan messages (block empty)
                    liftBase $ readMVar empty
                    waitUntil 10000000 $ findElem ( ById "tabShooting" ) >>= click
                    _ <- liftBase $ putMVar app' app''
                    app'' <- liftBase $ takeMVar app'
                    empty <- liftBase newEmptyMVar
                    liftBase $ writeChan messages (block empty)
                    liftBase $ readMVar empty
                    waitUntil 10000000 $ findElem ( ById "tabLocation" ) >>= click
                    _ <- liftBase $ putMVar app' app''
                    app'' <- liftBase $ takeMVar app'
                    empty <- liftBase newEmptyMVar
                    liftBase $ writeChan messages (block empty)
                    liftBase $ readMVar empty
                    waitUntil 10000000 $ findElem ( ById "tabMain" ) >>= click
                    liftBase $ putMVar app' app''
                    -}
           --         )
                

                {-
                liftBase $ forM_ [1..40] (\x -> do
                    writeFile ("test/files/input" ++ (show x) ++ ".CR2") (show [1..8000000]) -- create the source file
                    writeFile ("test/files/input" ++ (show x) ++ ".JPG") (show [1..8000000])-- create the source file
                    )

                liftBase $ setIdSelection config (Idd "")
                liftBase $ setBuilt' config (NoBuilt)

                liftBase $ threadDelay 4000000
                searchInput <- waitUntil 100000 $ findElem ( ById "fotoId" )
                sendKeys "1234" searchInput

                liftBase $ threadDelay 400000 -- vent for text alfredvestved
                --mover <- waitUntil 100000 $ findElem ( ById "builderButton" )
                --click mover
                searchInput2 <- waitUntil 10000 $ findElem ( ById "fotoId" )
                sendKeys enter searchInput2 

                liftBase $ putStrLn "gg2"
                liftBase $ threadDelay 30000000
                liftBase $ putStrLn "gg"
                liftBase $ threadDelay 100000
                liftBase $ putStrLn "gg"
                -}
                --msg <- waitUntil 1000000 $ findElem ( ById "result" )
                --result <- getText msg
                --expect (result == "Færdig")

                --WWHAT???
                --liftBase $ removeDirectoryRecursive "/home/magnus/Documents/projects/photoPenny/test/doneshooting/"
                --liftBase $ removeDirectoryRecursive "/home/magnus/Documents/projects/photoPenny/test/dagsdato/"


        {-
            _ <- liftBase $ setDump config $ NoDump -- reset or i will get stales
            _ <- liftBase $ setDagsdato config $ NoDagsdato -- reset or i will get stales
            _ <- liftBase $ setPhotographers config $ NoPhotographers -- reset or i will get stales
            _ <- liftBase $ setDoneshooting config $ NoDoneshooting -- reset or i will get stales
            _ <- liftBase $ setDagsdato config $ NoDagsdato -- reset or i will get stales
            _ <- liftBase $ setShooting config $ NoShootings -- reset or i will get stales
            _ <- liftBase $ setSession config $ NoSessions -- reset or i will get stales
            _ <- liftBase $ setLocation config $ NoLocation -- reset or i will get stales

            openPage "http://localhost:9000"
            --DUMP
            --DUMP
            tabDump <- waitUntil 100000 $ findElem ( ById "tabDump" ) 
            _ <- click tabDump
            _ <- liftBase $ setDump config $ Dump "testMappe" -- maybe i should inject this service
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "dumpOK")

            --DAGSDATO
            --DAGSDATO
            tabDagsdato <- waitUntil 100000 $ findElem ( ById "tabDagsdato" ) 
            _ <- click tabDagsdato
            _ <- liftBase $ setDagsdato config $ Dagsdato "testMappe" -- maybe i should inject this service
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "dagsdatoOK")

            --PHOTOGRAPHER
            --PHOTOGRAPHER
            tabPhotographer <- waitUntil 100000 $ findElem ( ById "tabPhotographer" ) 
            _ <- click tabPhotographer
            _ <- liftBase $ setPhotographers config $ Photographers (ListZipper [] (Photographer "Joe Test" "JT") []) -- maybe i should inject this service
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "photographerOK")
            
            --DONESHOOTING
            --DONESHOOTING
            tabDoneshooting <- waitUntil 100000 $ findElem ( ById "tabDoneshooting" ) 
            _ <- click tabDoneshooting
            _ <- liftBase $ setDoneshooting config $ Doneshooting "testMappe"
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "doneshootingOK")

            --SHOOTING
            --SHOOTING
            tabShooting <- waitUntil 100000 $ findElem ( ById "tabShooting" ) 
            _ <- click tabShooting
            _ <- liftBase $ setShooting config $ Shootings  (ListZipper [] Normal [])
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "shootingOK")
            
            --SESSION
            --SESSION
            tabSession <- waitUntil 100000 $ findElem ( ById "tabSession" ) 
            _ <- click tabSession
            _ <- liftBase $ setSession config $ Sessions (ListZipper [] School [])
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "sessionOK")

            --LOCATION
            --LOCATION
            tabLocation <- waitUntil 100000 $ findElem ( ById "tabLocation" ) 
            _ <- click tabLocation
            _ <- liftBase $ setLocation config $ Location "testlocation.csv"
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "locationOK")



            tabMain <- waitUntil 100000 $ findElem ( ById "tabMain" ) 
            _ <- click tabMain 
            searchInput <- waitUntil 10000 $ findElem ( ByCSS "input[type='text']" )  
            sendKeys "1234" searchInput
            mover <- waitUntil 100000 $ findElem ( ById "mover" )
            click mover
            msg <- findElem ( ById "result" )
            result <- getText msg
            expect (result == "Byg færdigt")
            _ <- liftBase $ putStrLn "complete"
            expect True
        )
        -}
