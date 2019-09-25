{-# LANGUAGE OverloadedStrings #-}
module Lib2
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
import qualified PhotoShake.Photographee2 as Photographee2
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


setupGradesListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO Notify.StopListening --- ??? STOP
setupGradesListener manager msgChan app = do -- THIS BAD
    let fpConfig = E._configs app
    let stateConfig = E._gradesFile app
    Notify.watchDir manager fpConfig 
        (\e -> takeFileName (Notify.eventPath e) == takeFileName stateConfig) (\_ -> writeChan msgChan Msg.getGrades)


setupStateListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO Notify.StopListening --- ??? STOP
setupStateListener manager msgChan app = do -- THIS BAD
    let fpConfig = E._configs app
    let stateConfig = E._stateFile app
    Notify.watchDir manager fpConfig 
        (\e -> takeFileName (Notify.eventPath e) == takeFileName stateConfig) (\_ -> writeChan msgChan Msg.getStates)


setupDumpListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO Notify.StopListening --- ??? STOP
setupDumpListener manager msgChan app = do -- THIS BAD
    let fpConfig = E._configs app
    let dumpConfig = E._dumpFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName dumpConfig) (\_ -> writeChan msgChan Msg.getDump)
        

setupDoneshootingListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO Notify.StopListening --- ??? STOP
setupDoneshootingListener manager msgChan app = do -- THIS BAD
    let fpConfig = E._configs app
    let doneshootingConfig = E._doneshootingFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName doneshootingConfig) (\_ -> writeChan msgChan Msg.getDoneshooting)


setupDagsdatoListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO Notify.StopListening --- ??? STOP
setupDagsdatoListener manager msgChan app = do -- THIS BAD
    let fpConfig = E._configs app
    let dagsdatoConfig = E._dagsdatoFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName dagsdatoConfig) (\_ -> writeChan msgChan Msg.getDagsdato)


setupDagsdatoBackupListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO Notify.StopListening --- ??? STOP
setupDagsdatoBackupListener manager msgChan app = do -- THIS BAD
    let fpConfig = E._configs app
    let dagsdatoBackupConfig = E._dagsdatoBackupFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName dagsdatoBackupConfig) (\_ -> writeChan msgChan Msg.getDagsdatoBackup)


setupPhotographerListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO Notify.StopListening --- ??? STOP
setupPhotographerListener manager msgChan app = do -- THIS BAD
    let fpConfig = E._configs app
    let photographerConfig = E._photographerFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName photographerConfig) (\_ -> writeChan msgChan Msg.getPhotographers)

setupShootingListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO Notify.StopListening --- ??? STOP
setupShootingListener manager msgChan app = do -- THIS BAD
    let fpConfig = E._configs app
    let shootingConfig = E._shootingFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName shootingConfig) (\_ -> writeChan msgChan Msg.getShootings)

setupSessionListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO Notify.StopListening --- ??? STOP
setupSessionListener manager msgChan app = do -- THIS BAD
    let fpConfig = E._configs app
    let sessionConfig = E._sessionFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName sessionConfig) (\_ -> writeChan msgChan Msg.getSessions)


setupLocationListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO Notify.StopListening --- ??? STOP
setupLocationListener manager msgChan app = do -- THIS BAD
    let fpConfig = E._configs app
    let locationConfig = E._locationFile app
    Notify.watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (Notify.eventPath e) == takeFileName locationConfig) (\_ -> writeChan msgChan Msg.getLocation)


setupLocationFileListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO Notify.StopListening --- ??? STOP
setupLocationFileListener manager msgChan app = do -- THIS BAD
    let fpConfig = E._configs app
    let location = E._location app
    Location.location (return (return ())) (\d -> do
        action <- mkDebounce defaultDebounceSettings
                 { debounceAction = writeChan msgChan Msg.getLocation
                 , debounceFreq = 1000000 -- 5 seconds
                 , debounceEdge = trailingEdge -- Trigger on the trailing edge
                 }
        Notify.watchDir manager (dropFileName d)
            (\e -> takeFileName (Notify.eventPath e) == takeFileName d) (\_ -> action)) location


setupIdListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO Notify.StopListening --- ??? STOP
setupIdListener manager msgChan app = do -- THIS BAD
    let fpConfig = E._configs app
    let idConfig = E._idFile app
    Notify.watchDir manager fpConfig 
        (\e -> takeFileName (Notify.eventPath e) == takeFileName idConfig) (\_ -> writeChan msgChan Msg.getId)


setupBuildListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO Notify.StopListening --- ??? STOP
setupBuildListener manager msgChan app = do -- THIS BAD
    let fpConfig = E._configs app
    let buildConfig = E._buildFile app
    Notify.watchDir manager fpConfig 
        (\e -> takeFileName (Notify.eventPath e) == takeFileName buildConfig) (\_ -> writeChan msgChan Msg.getBuild)


setupControlListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO Notify.StopListening --- ??? STOP
setupControlListener manager msgChan app = do -- THIS BAD
    let location = E._location app
    let doneshooting = E._doneshooting app
    let grades = E._grades app
    let cancelControl = return () --E._cancelControl app

    Location.location (return cancelControl) 
        (\l -> Doneshooting.doneshooting (return cancelControl)
            (\d -> Grade.grades (return cancelControl)
                (\g -> do
                    action <- mkDebounce defaultDebounceSettings
                             { debounceAction = writeChan msgChan Msg.getGrades
                             , debounceFreq = 1000000 -- 5 seconds
                             , debounceEdge = trailingEdge -- Trigger on the trailing edge
                             }
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


setupDumpFilesListener :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO Notify.StopListening --- ??? STOP
setupDumpFilesListener manager msgChan app = do -- THIS BAD
    let dump = E._dump app
    let cancelDumpFiles = return () --E._cancelDumpFiles app
    Dump.dump (return cancelDumpFiles) (\d -> do
        b <- doesDirectoryExist d
        action <- mkDebounce defaultDebounceSettings
                 { debounceAction = writeChan msgChan Msg.getDumpFiles
                 , debounceFreq = 1000000 -- 5 seconds
                 , debounceEdge = trailingEdge -- Trigger on the trailing edge
                 }
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
    _ <- writeChan msgs Msg.getSessions
    _ <- writeChan msgs Msg.getLocation
    _ <- writeChan msgs Msg.getGrades
    _ <- writeChan msgs Msg.getId
    _ <- writeChan msgs Msg.getDumpFiles
    _ <- writeChan msgs Msg.getBuild
    return ()

subscriptions :: Notify.WatchManager -> Chan Msg.Message -> E.App E.Model -> IO (Notify.StopListening, Notify.StopListening, Notify.StopListening, Notify.StopListening)
subscriptions manager msgs app = do
    state <- setupStateListener manager msgs app
    dump <- setupDumpListener manager msgs app
    doneshooting <- setupDoneshootingListener manager msgs app
    dagsdato <- setupDagsdatoListener manager msgs app
    dagsdatoBackup <- setupDagsdatoBackupListener manager msgs app
    photographer <- setupPhotographerListener manager msgs app
    shooting <- setupShootingListener manager msgs app
    session <- setupSessionListener manager msgs app
    location <- setupLocationListener manager msgs app
    locationFile <- setupLocationFileListener manager msgs app
    grade <- setupGradesListener manager msgs app
    id <- setupIdListener manager msgs app
    control <- setupControlListener manager msgs app
    dumpFiles <- setupDumpFilesListener manager msgs app
    build <- setupBuildListener manager msgs app
    return $ (msum 
        [ state
        , dump
        , doneshooting
        , dagsdato
        , dagsdatoBackup
        , photographer
        , shooting
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
                app' <- takeMVar app 
                --REAL SHITTY all the way
                let config = E._shakeConfig app'
                let photographee = E._photographee app'
                let location = E._location app'
                time <- liftIO $ getCurrentTime

                build <- Location.location (return ())  --dangerous as no change will cause bug
                        (\l -> maybe (return ()) (\p ->
                                myShake config p (takeBaseName l) time True) photographee) location

                let app'' = E._setStates app' Nothing -- i dont think i have to do this
                _ <- runUI w $ do
                    body <- getBody w
                    redoLayout body msgs app''
                putMVar app app'

            Msg.InsertPhotographee id name-> do                
                app' <- takeMVar app 
                --REAL SHITTY all the way
                let shakeConfig = E._shakeConfig app'
                location <- getLocationFile shakeConfig --wrong naming
                grades <- getGrades shakeConfig
                res <- Grade.grades (return Nothing) (\g -> Photographee2.insert location (focus g) id name) grades

                forM_ res (\_ -> runUI w $ do
                        let app'' = E._setStates app' Nothing -- i dont think i have to do this
                        body <- getBody w
                        redoLayout body msgs app'')
                putMVar app app'

            Msg.GetStates -> do                
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
                app' <- takeMVar app 
                let root = E._root app'
                let stateFile = E._stateFile app'
                _ <- interpret $ S.setStates (fp (unFP root =>> combine stateFile)) states
                let app'' = E._setStates app' Nothing -- i dont think i have to do this
                _ <- runUI w $ do
                    body <- getBody w
                    redoLayout body msgs app''
                putMVar app app'

            Msg.SetDump dump -> do
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setDump shakeConfig dump 
                let app'' = E._setStates app' Nothing -- i dont think i have to do this
                _ <- runUI w $ do
                    body <- getBody w
                    redoLayout body msgs app''
                putMVar app app'

            Msg.GetDump -> do
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let shakeConfig = E._shakeConfig app'
                dump <- getDump shakeConfig
                let app'' = E._setDump app' dump
                dumpFiles <- getDumpFiles dump
                let app''' = E._setDumpFiles app'' dumpFiles
                app'''' <- subs manager msgs app'''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app''''

                putMVar app app''''

            Msg.GetPhotographee -> do
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let location = E._location app'
                let id = E._id app'
                photographee <- Photographee2.findPhotographee location id
                let app'' = E._setPhotographee app' photographee
                app''' <- subs manager msgs app''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app'''
                putMVar app app'''

            Msg.GetDumpFiles -> do
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                _ <- E._cancelDumpFiles app'

                let dump = E._dump app'
                dumpFiles <- getDumpFiles dump
                let app'' = E._setDumpFiles app' dumpFiles

                app''' <- subs manager msgs app''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app'''

                putMVar app app'''

            Msg.SetDoneshooting doneshooting -> do
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setDoneshooting shakeConfig doneshooting
                let app'' = E._setStates app' Nothing -- i dont think i have to do this
                _ <- runUI w $ do
                    body <- getBody w
                    redoLayout body msgs app''
                putMVar app app'

            Msg.GetDoneshooting -> do
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
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setDagsdatoBackup shakeConfig dagsdato
                let app'' = E._setStates app' Nothing -- i dont think i have to do this
                _ <- runUI w $ do
                    body <- getBody w
                    redoLayout body msgs app''
                putMVar app app'

            Msg.GetDagsdatoBackup -> do
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
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setDagsdato shakeConfig dagsdato
                let app'' = E._setStates app' Nothing -- i dont think i have to do this
                _ <- runUI w $ do
                    body <- getBody w
                    redoLayout body msgs app''
                putMVar app app'

            Msg.GetDagsdato -> do
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
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setPhotographers shakeConfig photographers
                let app'' = E._setStates app' Nothing -- i dont think i have to do this
                _ <- runUI w $ do
                    body <- getBody w
                    redoLayout body msgs app''
                putMVar app app'

            Msg.GetPhotographers -> do
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
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setSession shakeConfig sessions -- should be called setShootings
                let app'' = E._setStates app' Nothing -- i dont think i have to do this
                _ <- runUI w $ do
                    body <- getBody w
                    redoLayout body msgs app''
                putMVar app app'

            Msg.GetSessions -> do
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

            Msg.SetShootings shootings -> do
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setShooting shakeConfig shootings -- should be called setShootings
                let app'' = E._setStates app' Nothing -- i dont think i have to do this
                _ <- runUI w $ do
                    body <- getBody w
                    redoLayout body msgs app''
                putMVar app app'

            Msg.GetShootings -> do
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
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setId shakeConfig id -- should be called setShootings
                let app'' = E._setStates app' Nothing -- i dont think i have to do this
                _ <- runUI w $ do
                    body <- getBody w
                    redoLayout body msgs app''
                putMVar app app'

            Msg.GetId -> do
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let shakeConfig = E._shakeConfig app'
                let location = E._location app'
                let id = E._id app'
                id <- getId shakeConfig 
                photographee <- Photographee2.findPhotographee location id
                let app'' = E._setId app' id
                let app''' = E._setPhotographee app'' photographee
                app'''' <- subs manager msgs app'''
                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app''''
                putMVar app app''''

            Msg.SetLocation location -> do
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setLocation shakeConfig location -- should be called setShootings
                let app'' = E._setStates app' Nothing -- i dont think i have to do this
                _ <- runUI w $ do
                    body <- getBody w
                    redoLayout body msgs app''
                putMVar app app'

            Msg.GetLocation -> do
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let shakeConfig = E._shakeConfig app'
                let id = E._id app'--wrong--wrong--wrong--wrong--wrong--wrong--wrong
                location <- getLocationFile shakeConfig --wrong naming
                photographee <- Photographee2.findPhotographee location id
                grades <- getGrades shakeConfig
                photographees <- Photographee2.fromGrade location grades
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
                app' <- takeMVar app 
                let shakeConfig = E._shakeConfig app'
                _ <- setGrades shakeConfig grades -- should be called setShootings
                let app'' = E._setStates app' Nothing -- i dont think i have to do this
                _ <- runUI w $ do
                    body <- getBody w
                    redoLayout body msgs app''
                putMVar app app'

            Msg.GetGrades -> do
                app' <- takeMVar app 
                _ <- E._cancel app'
                _ <- E._cancelDumpFiles app'
                _ <- E._cancelControl app'
                _ <- E._cancelLocation app'
                let shakeConfig = E._shakeConfig app'
                grades <- getGrades shakeConfig
                location <- getLocationFile shakeConfig --wrong naming
                control <- Grade.grades (return Control.Empty) (\zipper -> Control.controlXMP shakeConfig (extract zipper)) grades -- POORLY optimized
                photographees <- Photographee2.fromGrade location grades
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
            S.Shooting -> shootingSection body msgs states (E._shootings app)
            S.Location -> locationSection body msgs states (E._location app) (E._grades app)
            S.Main -> mainSection body msgs states (E._grades app) (E._id app) (E._dumpFiles app) (E._sessions app) (E._build app) (E._photographee app) (E._photographees app)
            S.Control -> controlSection body msgs states (E._grades app) (E._control app)
            --element body # set children [menu, view]



{-
dagsdatoBackupSection :: FilePath -> FilePath -> MVar States -> ListZipper State -> ShakeConfig -> MVar ShakeConfig -> UI Element
dagsdatoBackupSection  root stateFile states'' states config config' = do
    x <- liftIO $ withMVar config' $ (\conf -> getDagsdatoBackup conf)

    (_, view) <- mkFolderPicker "dagsDatoPicker" "Vælg config folder" $ \folder ->
        liftIO $ withMVar config' $ (\conf -> setDagsdatoBackup conf $ Dagsdato folder)

    dagsdato (mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dagsdato Backup mappe ikke valgt" ]
                            , mkColumn ["is-12"] [ element view ]
                            ]
                      ])
                (\y -> do
                    (buttonForward, forwardView) <- mkButton "next" "Ok"
                    on UI.click buttonForward $ \_ -> liftIO $ withMVar states'' $ (\_ ->  interpret $ setStates (mkFP root stateFile) (States (forward states)))

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Dagsdato backup mappe" # set (attr "id") "dagsdatoBackupOK" ]
                                    , mkColumn ["is-12"] [ element view ]
                                    , mkColumn ["is-12"] [ UI.p # set UI.text y ]
                                    , mkColumn ["is-12"] [ element forwardView ]
                                    ]
                              ] ) x
                              -}










--display :: Chan Message -> App FilePath -> Window -> UI Element
--display msgChan app w = do 
  --  state <- getStates msgChan --interpret $ getStates (mkFP root stateFile)

    --view <- case focus states of
   --         Main -> string "Main" 
     --       _ -> string "Other"

    --getBody w #+ [ ]

 

{-
viewState :: FilePath -> FilePath -> ShakeConfig -> Window -> Chan String -> Chan String -> Chan String -> MVar States -> MVar ShakeConfig -> ListZipper State -> UI (Element, Element)
viewState root stateFile config w chan chanPhotographer chanSession states'' config'' states = do
    tmp <- UI.div
    case (focus states) of 
            Dump -> do
                c <- dumpSection root stateFile states'' states config config''
                return (c,tmp)

            Dagsdato -> do
                c <- dagsdatoSection root stateFile states'' states config  config''
                return (c,tmp)

            Photographer -> do 
                c <- photographerSection root stateFile states'' states config config'' chanPhotographer
                return (c,tmp)

            Doneshooting -> do
                c <- doneshootingSection root stateFile states'' states config config''
                return (c,tmp)

            Session -> do
                c <- sessionSection root stateFile states'' states config config'' chanSession
                return (c,tmp)

            Shooting -> do
                c <- shootingSection root stateFile states'' states config config'' chan
                return (c,tmp)

            Location -> do
                c <- locationsSection root stateFile states'' states config config'' 
                return (c,tmp)

            Main -> mainSection root stateFile config config'' w

            Main2 -> mainSection2 root stateFile config config'' w

            Control -> do
                c <- controlSection root states'' config'' 
                return (c,tmp)



redoLayout2 :: Window -> FilePath -> FilePath -> ShakeConfig -> MVar ThreadId -> MVar ThreadId -> MVar States -> MVar ShakeConfig -> EventChannel -> MVar () -> UI ()
redoLayout2 w root stateFile config tid1 tid2 states'' config'' dumpChan layoutLock = void $ do 
    ehh <- liftIO $ forkIO $ do
                    dump <- liftIO $ withMVar config'' $ (\conf -> getDump conf)
                    withManager $ \mgr -> do
                            case dump of
                                    D.Dump x -> do
                                            watchDirChan
                                                    mgr
                                                    x --this is less wrong than earlier
                                                    (const True)
                                                    dumpChan
                                            forever $ threadDelay 1000000
                                    D.NoDump -> do
                                            return () --error "lol" -- return $ main c msgChan conf stateFile state 
                        --- ok
    forkId <- liftIO $ forkIO $ recevier2 w root config stateFile dumpChan tid1 tid2 states'' config'' layoutLock

    liftIO $ putMVar tid1 ehh
    liftIO $ putMVar tid2 forkId

    redoLayout w root stateFile config tid1 tid2 states'' config'' dumpChan layoutLock

redoLayout :: Window -> FilePath -> FilePath -> ShakeConfig -> MVar ThreadId -> MVar ThreadId -> MVar States -> MVar ShakeConfig -> EventChannel -> MVar () -> UI ()
redoLayout w root stateFile config tid1 tid2 states'' config'' dumpChan layoutLock = void $ do 
    --wauw much dubcha
    importText <- liftIO $ Chan.newChan
    importTextPhotographer <- liftIO $ Chan.newChan
    importTextSession <- liftIO $ Chan.newChan

    views <- liftIO $ withMVar states'' $ (\(States states) -> do
            return $ states =>> viewState root stateFile config w importText importTextPhotographer importTextSession states'' config'')


    view <- focus views
    buttons <- liftIO $ withMVar states'' $ (\(States states) -> return $ states =>> (\states' -> do
                        button <- UI.button # set (attr "id") ("tab" ++ show (focus states')) #. "button" #+ [string (show (focus states'))]
                        button' <- if (states' == states) then
                                set (UI.attr  "class") "button is-dark is-selected" (element button)
                            else
                                return button

                        on UI.click button' $ \_ -> do
                            liftIO $ withMVar states'' $ (\_ ->  interpret $ setStates (mkFP root stateFile) (States states'))

                        return button'
                    ))

    view'' <- mkSection [UI.div #. "buttons has-addons" #+ (toList buttons)]

    viewt <- mkSection []
    viewPhotographer <- mkSection []
    viewSession <- mkSection []

    --suchbads
    getBody w # set children [ view'', (fst view), viewt, viewPhotographer, viewSession]

    lol <- get value $ snd view
    --(Idd ident) <- liftIO $ withMVar config'' $ (\conf -> getIdSelection conf)
    UI.setFocus $ snd view
    UI.set value lol (element $ snd view)
    --wauw 

    messageReceiver <- liftIO $ forkIO $ receiveMessages w importText viewt
    messageReceiverPhotographer <- liftIO $ forkIO $ receiveMessagesPhotographer w importTextPhotographer viewPhotographer
    messageReceiverSession <- liftIO $ forkIO $ receiveMessagesSession w importTextSession viewSession


    liftIO $ putMVar layoutLock ()

    on UI.disconnect w $ const $ liftIO $ killThread messageReceiver

    on UI.disconnect w $ const $ liftIO $ killThread messageReceiverPhotographer 

    on UI.disconnect w $ const $ liftIO $ killThread messageReceiverSession 




receiveMessages :: Window -> (Chan String) -> Element -> IO ()
receiveMessages w msgs messageArea = do
    messages <- Chan.getChanContents msgs
    forM_ messages $ \msg -> do
        runUI w $ do
          element messageArea # set text msg
          flushCallBuffer

receiveMessagesPhotographer :: Window -> (Chan String) -> Element -> IO ()
receiveMessagesPhotographer w msgs messageArea = do
    messages <- Chan.getChanContents msgs
    forM_ messages $ \msg -> do
        runUI w $ do
          element messageArea # set text msg
          flushCallBuffer

receiveMessagesSession :: Window -> (Chan String) -> Element -> IO ()
receiveMessagesSession w msgs messageArea = do
    messages <- Chan.getChanContents msgs
    forM_ messages $ \msg -> do
        runUI w $ do
          element messageArea # set text msg
          flushCallBuffer


recevier  :: Window -> FilePath -> ShakeConfig -> FilePath -> EventChannel -> MVar ThreadId -> MVar ThreadId -> MVar States -> MVar ShakeConfig -> EventChannel -> MVar () -> IO ()
recevier w root config stateFile msgs tid1 tid2 stateLock configLock dumpChan layoutLock = void $ do 
    messages <- liftIO $ getChanContents msgs
    forM_ messages $ \_ -> do 
        liftIO $ takeMVar layoutLock

        conf <- liftIO $ takeMVar configLock
        liftIO $ putMVar configLock conf

        tt1 <- liftIO $ takeMVar tid1 
        liftIO $ killThread tt1
        tt2 <- liftIO $ takeMVar tid2
        liftIO $ killThread tt2

        liftIO $ modifyMVar_ stateLock $ (\_ ->  interpret $ getStates (mkFP root stateFile))
        runUI w $ do 
            redoLayout2 w root stateFile config tid1 tid2 stateLock configLock dumpChan layoutLock



recevier2  :: Window -> FilePath -> ShakeConfig -> FilePath -> EventChannel -> MVar ThreadId -> MVar ThreadId -> MVar States -> MVar ShakeConfig -> MVar () -> IO ()
recevier2 w root config stateFile msgs tid1 tid2 stateLock configLock layoutLock = void $ do
    messages <- Chan.getChanContents msgs

    drawAgain <- mkDebounce defaultDebounceSettings
                 { debounceAction = do 
                        liftIO $ takeMVar layoutLock
                        liftIO $ modifyMVar_ stateLock $ (\_ ->  interpret $ getStates (mkFP root stateFile))
                        conf <- liftIO $ takeMVar configLock
                        liftIO $ putMVar configLock conf
                        runUI w $ do
                            redoLayout w root stateFile config tid1 tid2 stateLock configLock msgs layoutLock
                 , debounceFreq = 3000000 -- 5 seconds
                 , debounceEdge = trailingEdge -- Trigger on the trailing edge
                 }

    forM_ messages $ \_ -> drawAgain 


-- eww
main :: App a -> ShakeConfig -> EventChannel -> FilePath -> FilePath -> States -> FilePath -> Window -> UI ()
main app config msgChan conf stateFile (States states) root w = do
    _ <- addStyleSheet w root "bulma.min.css"

    ggtid1 <- liftIO $ newEmptyMVar

    ggtid2 <- liftIO $ newEmptyMVar

    states' <- liftIO $ newMVar (States states)
    
    config' <- liftIO $ newMVar config

    layoutLock <- liftIO $ newEmptyMVar

    --wauw
    dumpChan <- liftIO $ Chan.newChan

    case focus states of
            Main -> starterScreen w root stateFile config states' config' ggtid1 ggtid2 layoutLock
            _ -> redoLayout2 w root stateFile config ggtid1 ggtid2 states' config' dumpChan layoutLock

    eh <- liftIO $ forkIO $ recevier w root config stateFile msgChan ggtid1 ggtid2 states' config' dumpChan layoutLock
    on UI.disconnect w $ const $ liftIO $ killThread eh



starterScreen :: Window -> FilePath -> FilePath -> ShakeConfig -> MVar States -> MVar ShakeConfig -> MVar ThreadId -> MVar ThreadId -> MVar () -> UI ()
starterScreen w root stateFile config states' config' tid1 tid2 layoutLock = void $ do

    dump <- dumpOverview root stateFile config config'
    dagsdato <- dagsdatoOverview root stateFile config config'
    photographer <- photographerOverview root stateFile config config'
    doneshooting <- doneshootingOverview root stateFile config config'
    session <- sessionOverview root stateFile config config' 
    shooting <- shootingOverview root stateFile config config' 
    location <- locationsOverview root stateFile config config'

    (buttonForward, forwardView) <- mkButton "next" "Ok"
    on UI.click buttonForward $ \_ -> liftIO $ do
            withMVar states' $ (\states ->  interpret $ setStates (mkFP root stateFile) states)

    view' <- mkSection [ element forwardView]

    view <- mkSection [ element dump 
                      , element dagsdato
                      , element doneshooting
                      , element session
                      , element photographer
                      , element shooting
                      , element location
                      , element view'
                      ]

    getBody w # set children [ view ]

    t1 <- liftIO $ forkIO $ forever $ threadDelay 1000000
    t2 <- liftIO $ forkIO $ forever $ threadDelay 1000000

    liftIO $ putMVar tid1 t1
    liftIO $ putMVar tid2 t2
    liftIO $ putMVar layoutLock ()




-- kinda of bad
missingConf :: SomeException -> FilePath -> Window -> UI ()
missingConf e root w = do
    _ <- addStyleSheet w root "bulma.min.css"
    section <- mkSection [UI.p # set UI.text ("Mangler måske config" ++ (show e))]
    _ <- getBody w #+ [element section] 
    return ()
    
-}
