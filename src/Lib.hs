{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( setup
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import State
import Elements

import PhotoShake.ShakeConfig
import qualified PhotoShake.Dump as D

import System.FilePath

import Summary
import Main
import Dump
import Dagsdato
import Photographer
import Session
import Shooting
import Doneshooting
import Locations


import Control.Monad 

import Control.Exception

import System.FSNotify hiding (defaultConfig)
import Control.Concurrent

import qualified Control.Concurrent.Chan as Chan

import Utils.Comonad
import Utils.ListZipper


--use reader with env
setup :: Int -> String -> String -> FilePath -> FilePath -> IO ()
setup port root conf watchDir' stateFile = do
    config <- try $ toShakeConfig (Just root) conf :: IO (Either SomeException ShakeConfig)
    -- get appState.
    state <- getStates root stateFile

    withManager $ \mgr -> do
            msgChan <- newChan

            _ <- watchDirChan
                    mgr
                    (root </> watchDir') --this is less wrong than earlier
                    (const True)
                    msgChan

            view <- case config of 
                    Right c -> do
                        return $ main c msgChan conf stateFile state 

                    Left xxx -> 
                        return $ missingConf xxx

            startGUI
                defaultConfig { jsPort = Just port
                              , jsWindowReloadOnDisconnect = False
                              } (view root)
 


viewState :: FilePath -> FilePath -> ShakeConfig -> Window -> Chan String -> Chan String -> Chan String -> MVar States -> MVar ShakeConfig -> ListZipper State -> UI Element
viewState root stateFile config w chan chanPhotographer chanSession states'' config'' states = do
    case (focus states) of 

            Dump -> dumpSection root stateFile states'' states config config''

            Dagsdato -> dagsdatoSection root stateFile states'' states config  config''

            Photographer -> photographerSection root stateFile states'' states config config'' chanPhotographer

            Doneshooting -> doneshootingSection root stateFile states'' states config config''

            Session -> sessionSection root stateFile states'' states config config'' chanSession

            Shooting -> shootingSection root stateFile states'' states config config'' chan

            Location -> locationsSection root stateFile states'' states config config'' 

            Main -> mainSection root stateFile config config'' w





redoLayout :: Window -> FilePath -> FilePath -> ShakeConfig -> TVar ThreadId -> TVar ThreadId -> MVar States -> MVar ShakeConfig -> UI ()
redoLayout w root stateFile config tid1 tid2 states'' config'' = void $ do 

    (States states) <- liftIO $ readMVar states''

    --wauw much dubcha
    importText <- liftIO $ Chan.newChan
    importTextPhotographer <- liftIO $ Chan.newChan
    importTextSession <- liftIO $ Chan.newChan
    --wauw
    dumpChan <- liftIO $ Chan.newChan

    let views = states =>> viewState root stateFile config w importText importTextPhotographer importTextSession states'' config''


    view <- focus views
    let buttons = states =>> (\states' -> do
                        button <- UI.button # set (attr "id") ("tab" ++ show (focus states')) #. "button" #+ [string (show (focus states'))]
                        button' <- if (states' == states) then
                                set (UI.attr  "class") "button is-info is-selected" (element button)
                            else
                                return button

                        on UI.click button' $ \_ -> do
                            liftIO $ withMVar states'' $ (\_ -> setStates root stateFile (States states'))

                        return button'
                    )

    view'' <- mkSection [UI.div #. "buttons has-addons" #+ (toList buttons)]

    viewt <- mkSection []
    viewPhotographer <- mkSection []
    viewSession <- mkSection []

    --suchbads
    getBody w # set children [ view'', view, viewt, viewPhotographer, viewSession]

    --wauw 

    messageReceiver <- liftIO $ forkIO $ receiveMessages w importText viewt
    messageReceiverPhotographer <- liftIO $ forkIO $ receiveMessagesPhotographer w importTextPhotographer viewPhotographer
    messageReceiverSession <- liftIO $ forkIO $ receiveMessagesSession w importTextSession viewSession

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
    forkId <- liftIO $ forkIO $ recevier2 w root config stateFile dumpChan tid1 tid2 states'' config''

    liftIO $ atomically $ writeTVar tid1 ehh
    liftIO $ atomically $ writeTVar tid2 forkId

    on UI.disconnect w $ const $ liftIO $ killThread ehh

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


recevier  :: Window -> FilePath -> ShakeConfig -> FilePath -> EventChannel -> TVar ThreadId -> TVar ThreadId -> MVar States -> MVar ShakeConfig -> IO ()
recevier w root config stateFile msgs tid1 tid2 stateLock configLock = void $ do 
    messages <- liftIO $ getChanContents msgs
    forM_ messages $ \_ -> do 
        tid1' <- liftIO $ atomically $ readTVar tid1
        tid2' <- liftIO $ atomically $ readTVar tid2
        liftIO $ modifyMVar_ stateLock $ (\_ -> getStates root stateFile)
        runUI w $ do 
            redoLayout w root stateFile config tid1 tid2 stateLock configLock
            liftIO $ killThread tid1'
            liftIO $ killThread tid2'


recevier2  :: Window -> FilePath -> ShakeConfig -> FilePath -> EventChannel -> TVar ThreadId -> TVar ThreadId -> MVar States -> MVar ShakeConfig -> IO ()
recevier2 w root config stateFile msgs tid1 tid2 stateLock configLock = void $ do
    messages <- Chan.getChanContents msgs
    forM_ messages $ \msg -> do
        tid1' <- liftIO $ atomically $ readTVar tid1
        tid2' <- liftIO $ atomically $ readTVar tid2
        liftIO $ modifyMVar_ stateLock $ (\_ -> getStates root stateFile)
        runUI w $ do
            redoLayout w root stateFile config tid1 tid2 stateLock configLock
            liftIO $ killThread tid1'
            liftIO $ killThread tid2'


-- eww
main :: ShakeConfig -> EventChannel -> FilePath -> FilePath -> States -> FilePath -> Window -> UI ()
main config msgChan conf stateFile (States states) root w = do
    _ <- addStyleSheet w root "bulma.min.css"

    tid1 <- liftIO $ forkIO $ return ()
    ggtid1 <- liftIO $ atomically $ newTVar tid1

    tid2 <- liftIO $ forkIO $ return ()
    ggtid2 <- liftIO $ atomically $ newTVar tid2

    states' <- liftIO $ newMVar (States states)
    
    config' <- liftIO $ newMVar config

    case focus states of
            Main -> starterScreen w root stateFile config states' config'
            _ -> redoLayout w root stateFile config ggtid1 ggtid2 states' config'

    eh <- liftIO $ forkIO $ recevier w root config stateFile msgChan ggtid1 ggtid2 states' config'
    on UI.disconnect w $ const $ liftIO $ killThread eh



starterScreen :: Window -> FilePath -> FilePath -> ShakeConfig -> MVar States -> MVar ShakeConfig -> UI ()
starterScreen w root stateFile config states' config' = void $ do

    dump <- dumpOverview root stateFile config config'
    dagsdato <- dagsdatoOverview root stateFile config config'
    photographer <- photographerOverview root stateFile config config'
    doneshooting <- doneshootingOverview root stateFile config config'
    session <- sessionOverview root stateFile config config' 
    shooting <- shootingOverview root stateFile config config' 
    location <- locationsOverview root stateFile config config'

    (buttonForward, forwardView) <- mkButton "next" "Ok"
    on UI.click buttonForward $ \_ -> liftIO $ do
            withMVar states' $ (\states -> setStates root stateFile states)

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




-- kinda of bad
missingConf :: SomeException -> FilePath -> Window -> UI ()
missingConf e root w = do
    _ <- addStyleSheet w root "bulma.min.css"
    section <- mkSection [UI.p # set UI.text ("Mangler måske config" ++ (show e))]
    _ <- getBody w #+ [element section] 
    return ()
    

