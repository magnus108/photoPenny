{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( main
    , initialMessage
    , subscriptions
    ) where

import Debug.Trace
import Data.Time.Clock

import Utils.Debounce

import System.Directory

import Control.Concurrent.MVar

import qualified PhotoShake.State as S
import Utils.ListZipper
import Utils.FP
import Utils.Comonad
import Utils.Actions


import qualified Model.E as E
import qualified Message as Msg

import Elements

import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Monad 

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import qualified System.FSNotify as Notify
import System.FilePath hiding (combine)


import Data.Function ((&))

import Dump
import Doneshooting
import Camera
import Dagsdato
import DagsdatoBackup
import Shooting
import Session
import Photographer
import Locations -- wrong name
import Control
import Main

import Control.Exception
import PhotoShake
import PhotoShake.ShakeConfig 
import qualified PhotoShake.Grade as Grade
import qualified PhotoShake.Dump as Dump
import qualified PhotoShake.Photographee as Photographee
import PhotoShake.Doneshooting hiding (setDoneshooting, getDoneshooting) --this correcto
import qualified PhotoShake.Doneshooting as Doneshooting --this correcto
import qualified PhotoShake.Location as Location --this correcto
import PhotoShake.Dagsdato
import PhotoShake.Photographer hiding (setPhotographers, getPhotographers)

import qualified PhotoShake.Control as Control


main :: Int -> Notify.WatchManager -> Chan Msg.Message -> MVar (E.App E.Model) -> IO ()
main port manager messages app = do 
    setupSubscriptions <- withMVar app (return . E._subscriptions)
    (cancel, dumpFiles, control, location) <- withMVar app (setupSubscriptions manager messages)
    _ <- modifyMVar_ app $ \a -> return (E._setCancel a cancel)
    _ <- modifyMVar_ app $ \a -> return (E._setCancelDumpFiles a dumpFiles)
    _ <- modifyMVar_ app $ \a -> return (E._setCancelControl a control)
    _ <- modifyMVar_ app $ \a -> return (E._setCancelLocation a location)
    
    startGUI defaultConfig { jsPort = Just port
                           , jsWindowReloadOnDisconnect = False
                           } $ setup manager messages app


setupGradesListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO () -> IO Notify.StopListening --- ??? STOP
setupGradesListener manager msgChan app action = do -- THIS BAD
    let fpConfig = E._configs app
    let gradeConfig = E._gradesFile app
    Notify.watchDir manager fpConfig 
        (\e -> takeFileName (Notify.eventPath e) == takeFileName gradeConfig) (\_ -> action ) 


setupStateListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO () -> IO Notify.StopListening --- ??? STOP
setupStateListener manager msgChan app action = do -- THIS BAD
    let fpConfig = E._configs app
    let stateConfig = E._stateFile app
    Notify.watchDir manager fpConfig 
        (\e -> takeFileName (Notify.eventPath e) == takeFileName stateConfig) (\_ -> action )


setupDumpListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO () -> IO Notify.StopListening --- ??? STOP
setupDumpListener manager msgChan app action = do -- THIS BAD
    let fpConfig = E._configs app
    let dumpConfig = E._dumpFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName dumpConfig) (\_ -> action)
        

setupDoneshootingListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO () -> IO Notify.StopListening --- ??? STOP
setupDoneshootingListener manager msgChan app action = do -- THIS BAD
    let fpConfig = E._configs app
    let doneshootingConfig = E._doneshootingFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName doneshootingConfig) (\_ -> action )


setupDagsdatoListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO () -> IO Notify.StopListening --- ??? STOP
setupDagsdatoListener manager msgChan app action = do -- THIS BAD
    let fpConfig = E._configs app
    let dagsdatoConfig = E._dagsdatoFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName dagsdatoConfig) (\_ -> action) 


setupDagsdatoBackupListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO () -> IO Notify.StopListening --- ??? STOP
setupDagsdatoBackupListener manager msgChan app action = do -- THIS BAD
    let fpConfig = E._configs app
    let dagsdatoBackupConfig = E._dagsdatoBackupFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName dagsdatoBackupConfig) (\_ -> action)


setupPhotographerListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO () -> IO Notify.StopListening --- ??? STOP
setupPhotographerListener manager msgChan app action = do -- THIS BAD
    let fpConfig = E._configs app
    let photographerConfig = E._photographerFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName photographerConfig) (\_ -> action) 

setupCamerasListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO () -> IO Notify.StopListening --- ??? STOP
setupCamerasListener manager msgChan app action = do -- THIS BAD
    let fpConfig = E._configs app
    let camerasConfig = E._camerasFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName camerasConfig) (\_ -> action) 

setupShootingListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO () -> IO Notify.StopListening --- ??? STOP
setupShootingListener manager msgChan app action = do -- THIS BAD
    let fpConfig = E._configs app
    let shootingConfig = E._shootingFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName shootingConfig) (\_ -> action) 

setupSessionListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO () -> IO Notify.StopListening --- ??? STOP
setupSessionListener manager msgChan app action = do -- THIS BAD
    let fpConfig = E._configs app
    let sessionConfig = E._sessionFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName sessionConfig) (\_ -> action) 


setupLocationListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO () -> IO Notify.StopListening --- ??? STOP
setupLocationListener manager msgChan app action = do -- THIS BAD
    let fpConfig = E._configs app
    let locationConfig = E._locationFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName locationConfig) (\_ -> action) 


setupLocationFileListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO () -> IO Notify.StopListening --- ??? STOP
setupLocationFileListener manager msgChan app action = do -- THIS BAD
    let fpConfig = E._configs app
    let location = E._location app
    Location.location (return (return ())) (\d -> do
        Notify.watchDir manager (dropFileName d)
            (\e -> takeFileName (Notify.eventPath e) == takeFileName d) (\_ -> action)) location


setupIdListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO () -> IO Notify.StopListening --- ??? STOP
setupIdListener manager msgChan app action = do -- THIS BAD
    let fpConfig = E._configs app
    let idConfig = E._idFile app
    Notify.watchDir manager fpConfig 
        (\e -> takeFileName (Notify.eventPath e) == takeFileName idConfig) (\_ -> action)


setupBuildListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO () -> IO Notify.StopListening --- ??? STOP
setupBuildListener manager msgChan app action = do -- THIS BAD
    let fpConfig = E._configs app
    let buildConfig = E._buildFile app
    Notify.watchDir manager fpConfig 
        (\e -> takeFileName (Notify.eventPath e) == takeFileName buildConfig) (\_ -> action)


setupControlListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO () -> IO Notify.StopListening --- ??? STOP
setupControlListener manager msgChan app action = do -- THIS BAD
    let location = E._location app
    let doneshooting = E._doneshooting app
    let grades = E._grades app
    let cancelControl = return () --E._cancelControl app

    Location.location (return cancelControl) 
        (\l -> Doneshooting.doneshooting (return cancelControl)
            (\d -> Grade.grades (return cancelControl)
                (\g -> do
                    b <- doesDirectoryExist (d </> (takeBaseName l) </> "cr2" </> (extract g))
                    if b then
                        Notify.watchDir manager (d </> (takeBaseName l) </> "cr2" </> (extract g)) 
                            (\x -> (eventNotModified x && (not (Notify.eventIsDirectory x)))) (\_ -> action)
                    else
                        return (cancelControl)) grades ) doneshooting) location

eventNotModified :: Notify.Event -> Bool
eventNotModified (Notify.Added    _ _ _) = True
eventNotModified (Notify.Modified _ _ _) = False
eventNotModified (Notify.Removed  _ _ _) = True
eventNotModified (Notify.Unknown  _ _ _) = True


setupDumpFilesListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO () -> IO Notify.StopListening --- ??? STOP
setupDumpFilesListener manager msgChan app action = do -- THIS BAD
    let dump = E._dump app
    let cancelDumpFiles = return () --E._cancelDumpFiles app
    Dump.dump (return cancelDumpFiles) (\d -> do
        b <- doesDirectoryExist d
        if b then 
            Notify.watchDir manager d (\x -> eventNotModified x) (\_ -> action)
        else
            return cancelDumpFiles) dump


initialMessage :: Chan Msg.Message -> IO ()
initialMessage msgs = do
    _ <- writeChan msgs Msg.getStates 
    _ <- writeChan msgs Msg.getDump 
    _ <- writeChan msgs Msg.getDoneshooting
    _ <- writeChan msgs Msg.getDagsdato
    _ <- writeChan msgs Msg.getDagsdatoBackup
    _ <- writeChan msgs Msg.getPhotographers
    _ <- writeChan msgs Msg.getShootings
    _ <- writeChan msgs Msg.getCameras
    _ <- writeChan msgs Msg.getSessions
    _ <- writeChan msgs Msg.getLocation
    _ <- writeChan msgs Msg.getGrades
    _ <- writeChan msgs Msg.getId
    _ <- writeChan msgs Msg.getDumpFiles
    _ <- writeChan msgs Msg.getBuild
    return ()

subscriptions :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO (Notify.StopListening, Notify.StopListening, Notify.StopListening, Notify.StopListening)
subscriptions manager msgs app = do
    let actionDumpFiles = E._actionDumpFiles app
    let actionGetBuild = E._actionGetBuild app

    let actionLocation = E._actionLocation app
    let actionGrades = E._actionGrades app
    let actionGrades2 = E._actionGrades2 app

    let actionId = E._actionId app

    let actionSession = E._actionSession app
    let actionShooting = E._actionShooting app
    let actionCameras = E._actionCameras app
    let actionPhotographer = E._actionPhotographer app

    let actionDagsdato = E._actionDagsdato app
    let actionDagsdatoBackup = E._actionDagsdatoBackup app
    let actionDoneshooting = E._actionDoneshooting app

    let actionDump = E._actionDump app
    let actionState = E._actionState app

    state <- setupStateListener manager msgs app actionState
    dump <- setupDumpListener manager msgs app actionDump
    doneshooting <- setupDoneshootingListener manager msgs app actionDoneshooting
    dagsdato <- setupDagsdatoListener manager msgs app actionDagsdato
    dagsdatoBackup <- setupDagsdatoBackupListener manager msgs app actionDagsdatoBackup
    photographer <- setupPhotographerListener manager msgs app actionPhotographer
    shooting <- setupShootingListener manager msgs app actionShooting
    cameras <- setupCamerasListener manager msgs app actionCameras
    session <- setupSessionListener manager msgs app actionSession
    location <- setupLocationListener manager msgs app actionLocation
    locationFile <- setupLocationFileListener manager msgs app actionLocation
    grade <- setupGradesListener manager msgs app actionGrades 
    id <- setupIdListener manager msgs app actionId
    control <- setupControlListener manager msgs app actionGrades2
    dumpFiles <- setupDumpFilesListener manager msgs app actionDumpFiles
    build <- setupBuildListener manager msgs app actionGetBuild
    return $ (msum 
        [ state
        , dump
        , doneshooting
        , dagsdato
        , dagsdatoBackup
        , photographer
        , shooting
        , cameras
        , session
        , location
        , grade
        , id
        , build
        ]
        , dumpFiles
        , control
        , locationFile
        )

setup :: Notify.WatchManager -> Chan Msg.Message -> MVar (E.App E.Model) -> Window -> UI ()
setup manager msgs app w = do

    receiver <- liftIO $ forkIO $ receive manager msgs app w

    on UI.disconnect w $ const $ liftIO $ killThread receiver
    on UI.disconnect w $ const $ liftIO $ Notify.stopManager manager

subs :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO (E.App E.Model)
subs manager msgs app = do 
    let setupSubscriptions = E._subscriptions app
    (cancel, dumpFiles, control, location) <- setupSubscriptions manager msgs app 
    let app'' = E._setCancel app cancel
    let app''' = E._setCancelDumpFiles app'' dumpFiles
    let app'''' = E._setCancelControl app''' control
    let app''''' = E._setCancelLocation app'''' location
    return app'''''


receive :: Notify.WatchManager -> Chan Msg.Message -> MVar (E.App E.Model) -> Window -> IO ()
receive manager msgs app w = do
    messages <- getChanContents msgs
    
    forM_ messages $ \ msg -> do 
        
        -- cancel subscriptsions 
        -- setup new ones

        case msg of
            Msg.GetBuild -> do                
                putStrLn "bobGetBuild"
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let shakeConfig = E._shakeConfig app'
                build <- getBuild shakeConfig
                let app'' = E._setBuild app' build


                app''' <- subs manager msgs app''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app'''

                putMVar app app'''

            Msg.Build -> do                
                putStrLn "build"
                app' <- takeMVar app 
                --REAL SHITTY all the way
                let config = E._shakeConfig app'
                let photographee = E._photographee app'
                let location = E._location app'
                time <- liftIO $ getCurrentTime

                build <- Location.location (return ())  --dangerous as no change will cause bug
                        (\l -> maybe (return ()) (\p ->
                                myShake config p (takeBaseName l) time True) photographee) location

                putMVar app app'

            Msg.InsertPhotographee id name-> do                
                putStrLn "insertPhotographee"
                app' <- takeMVar app 
                --REAL SHITTY all the way
                let shakeConfig = E._shakeConfig app'
                location <- getLocationFile shakeConfig --wrong naming
                grades <- getGrades shakeConfig
                let res = Grade.grades Nothing (\g -> Photographee.insert location (focus g) id name) grades
                putMVar app app'

            Msg.GetStates -> do                
                putStrLn "bobGetStates"
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let root = E._root app'
                let stateFile = E._stateFile app'
                states <- interpret $ S.getStates $ fp $ unFP root =>> combine stateFile
                let app'' = E._setStates app' (Just states)

                app''' <- subs manager msgs app''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app'''

                putMVar app app'''

            Msg.SetStates states -> do
                putStrLn "setStates"
                app' <- takeMVar app 
                let root = E._root app'
                let stateFile = E._stateFile app'

                _ <- interpret $ S.setStates (fp (unFP root =>> combine stateFile)) states
                putMVar app app'

            Msg.SetDump dump -> do
                putStrLn "setDump"
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setDump shakeConfig dump 
                putMVar app app'

            Msg.GetDump -> do
                putStrLn "bobGetDump"
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let shakeConfig = E._shakeConfig app'
                let cameras = E._cameras app'
                dump <- getDump shakeConfig
                let app'' = E._setDump app' dump
                dumpFiles <- getDumpFiles dump cameras
                let app''' = E._setDumpFiles app'' dumpFiles
                app'''' <- subs manager msgs app'''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app''''

                putMVar app app''''


            Msg.GetDumpFiles -> do
                putStrLn "bobGetDumpFiles"
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                _ <- E._cancelDumpFiles app'

                let cameras = E._cameras app'
                let dump = E._dump app'
                dumpFiles <- getDumpFiles dump cameras
                let app'' = E._setDumpFiles app' dumpFiles
            

                app''' <- subs manager msgs app''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app'''

                putMVar app app'''

            Msg.SetDoneshooting doneshooting -> do
                putStrLn "setDoneshooting"
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setDoneshooting shakeConfig doneshooting
                putMVar app app'

            Msg.GetDoneshooting -> do
                putStrLn "bobGetDoneshooting"
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let shakeConfig = E._shakeConfig app'
                doneshooting <- getDoneshooting shakeConfig
                let app'' = E._setDoneshooting app' doneshooting
                app''' <- subs manager msgs app''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app'''

                putMVar app app'''

            Msg.SetDagsdatoBackup dagsdato -> do
                putStrLn "setDagsdato"
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setDagsdatoBackup shakeConfig dagsdato
                putMVar app app'

            Msg.GetDagsdatoBackup -> do
                putStrLn "bobGetDasdatoBackup"
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let shakeConfig = E._shakeConfig app'
                dagsdatoBackup <- getDagsdatoBackup shakeConfig
                let app'' = E._setDagsdatoBackup app' dagsdatoBackup
                app''' <- subs manager msgs app''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app'''

                putMVar app app'''

            Msg.SetDagsdato dagsdato -> do
                putStrLn "bobDagsdato"
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setDagsdato shakeConfig dagsdato
                putMVar app app'

            Msg.GetDagsdato -> do
                putStrLn "bobGetDagsdato"
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let shakeConfig = E._shakeConfig app'
                dagsdato <- getDagsdato shakeConfig
                let app'' = E._setDagsdato app' dagsdato
                app''' <- subs manager msgs app''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app'''

                putMVar app app'''

            Msg.SetPhotographers photographers -> do
                putStrLn "setPhotographers"
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setPhotographers shakeConfig photographers
                putMVar app app'

            Msg.GetPhotographers -> do
                putStrLn "bobGetPhotographers"
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let shakeConfig = E._shakeConfig app'
                photographers <- getPhotographers shakeConfig
                let app'' = E._setPhotographers app' photographers
                app''' <- subs manager msgs app''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app'''

                putMVar app app'''

            Msg.SetSessions sessions -> do
                putStrLn "setSessions"
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setSession shakeConfig sessions -- should be called setShootings
                putMVar app app'

            Msg.GetSessions -> do
                putStrLn "bobGetSessions"
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let shakeConfig = E._shakeConfig app'
                sessions <- getSessions shakeConfig
                let app'' = E._setSessions app' sessions
                app''' <- subs manager msgs app''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app'''
                putMVar app app'''
            
            Msg.SetCameras cameras -> do
                putStrLn "setcameras"
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setCameras shakeConfig cameras -- should be called setShootings
                putMVar app app'

            Msg.GetCameras -> do
                putStrLn "bobGetCameras"
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let shakeConfig = E._shakeConfig app'
                cameras <- getCameras shakeConfig
                let app'' = E._setCameras app' cameras 
                app''' <- subs manager msgs app''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app'''
                putMVar app app'''

            Msg.SetShootings shootings -> do
                putStrLn "setshootings"
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setShooting shakeConfig shootings -- should be called setShootings
                putMVar app app'

            Msg.GetShootings -> do
                putStrLn "bobGetShootings"
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let shakeConfig = E._shakeConfig app'
                shootings <- getShootings shakeConfig
                let app'' = E._setShootings app' shootings
                app''' <- subs manager msgs app''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app'''
                putMVar app app'''

            Msg.SetId id -> do
                putStrLn "setId"
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setId shakeConfig id -- should be called setShootings
                putMVar app app'

            Msg.GetId -> do
                putStrLn "bobGetId"
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let shakeConfig = E._shakeConfig app'
                let location = E._location app'
                let id = E._id app'
                id <- getId shakeConfig 
                photographee <- Photographee.findPhotographee location id
                let app'' = E._setId app' id
                let app''' = E._setPhotographee app'' photographee
                app'''' <- subs manager msgs app'''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app''''
                putMVar app app''''

            Msg.SetLocation location -> do
                putStrLn "setLocation"
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setLocation shakeConfig location -- should be called setShootings
                putMVar app app'

            Msg.GetLocation -> do
                putStrLn "bobGetLocation"
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let shakeConfig = E._shakeConfig app'
                let id = E._id app'--wrong--wrong--wrong--wrong--wrong--wrong--wrong
                location <- getLocationFile shakeConfig --wrong naming
                photographee <- Photographee.findPhotographee location id
                grades <- getGrades shakeConfig
                photographees <- Photographee.fromGrade location grades
                let app'' = E._setPhotographee app' photographee
                let app''' = E._setLocation app'' location
                let app'''' = E._setPhotographees app''' photographees 
                app''''' <- subs manager msgs app''''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app'''''
                putMVar app app'''''

            Msg.SetGrades grades -> do
                putStrLn "setGrades"
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setGrades shakeConfig grades -- should be called setShootings
                putMVar app app'

            Msg.GetGrades -> do
                putStrLn "bobGetGrades"
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let shakeConfig = E._shakeConfig app'
                grades <- getGrades shakeConfig
                location <- getLocationFile shakeConfig --wrong naming
                control <- Grade.grades (return Control.Empty) (\zipper -> Control.controlXMP shakeConfig (extract zipper)) grades -- POORLY optimized
                photographees <- Photographee.fromGrade location grades
                let app'' = E._setGrades app' grades
                let app''' = E._setControl app'' control
                let app'''' = E._setPhotographees app''' photographees
                app''''' <- subs manager msgs app''''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app'''''
                putMVar app app'''''
    
            -- not sure about this..
            Msg.Block x -> do -- i can maybe do something good with this.
                app' <- takeMVar app 
                let app'' = E._setStates app' Nothing -- i dont think i have to do this
                _ <- runUI w $ do
                    body <- getBody w
                    redoLayout body msgs app''
                _ <- runUI w $ do
                    body <- getBody w
                    redoLayout body msgs app'
                putMVar app app'
                putMVar x () 



redoLayout :: Element -> Chan Msg.Message -> E.App E.Model -> UI ()
redoLayout body msgs app = void $ do
    
    let states = E._states app

    case states of
        Just (S.States states) -> do
            states =>> viewState body msgs app & focus 

        Nothing -> do
            element body # set children []
            return ()


--viewState :: Element -> Chan Msg.Message -> App Model > UI (Element, Element)
viewState :: Element -> Chan Msg.Message -> E.App E.Model -> ListZipper S.State -> UI ()
viewState body msgs app states = do
    let state = focus states
    case state of 
            S.Dump -> dumpSection body msgs states (E._dump app)
            S.Doneshooting -> doneshootingSection body msgs states (E._doneshooting app)
            S.Dagsdato -> dagsdatoSection body msgs states (E._dagsdato app)
            S.DagsdatoBackup -> dagsdatoBackupSection body msgs states (E._dagsdatoBackup app)
            S.Photographer -> photographerSection body msgs states (E._photographers app)
            S.Session -> sessionSection body msgs states (E._sessions app)
            S.Camera -> cameraSection body msgs states (E._cameras app)
            S.Shooting -> shootingSection body msgs states (E._shootings app)
            S.Location -> locationSection body msgs states (E._location app) (E._grades app)
            S.Main -> mainSection body msgs states (E._grades app) (E._id app) (E._dumpFiles app) (E._sessions app) (E._build app) (E._photographee app) (E._photographees app)
            S.Control -> controlSection body msgs states (E._grades app) (E._control app)
