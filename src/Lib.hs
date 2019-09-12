{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( setup
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import PhotoShake.State
import Elements

import Utils.Debounce
import PhotoShake.ShakeConfig
import qualified PhotoShake.Dump as D

import System.FilePath

import Summary
import Main
import Main2
import Dump
import Dagsdato
import DagsdatoBackup
import Photographer
import Session
import Shooting
import Doneshooting
import DoneshootingBackup
import Locations
import Control


import PhotoShake.Photographee
import Control.Monad 

import Control.Exception

import System.FSNotify hiding (defaultConfig)
import Control.Concurrent

import qualified Control.Concurrent.Chan as Chan

import Utils.Env
import Utils.Comonad
import Utils.ListZipper
import Utils.Actions
import Utils.FP


data E 
    = Production Configuration 
    | Test Configuration

data Configuration = Configuration
    { root :: String
    }

newtype App a = App { unApp :: Env E a }


--use reader with env
setup :: Int -> String -> String -> FilePath -> FilePath -> IO ()
setup port root conf watchDir' stateFile = do
    config <- try $ toShakeConfig (Just root) conf :: IO (Either SomeException ShakeConfig)
    -- get appState.

    -----
    let prod = Production (Configuration root) 
    let app = App (env prod 1)
    -----
    state <- interpret $ getStates (mkFP root stateFile)

    withManager $ \mgr -> do
            msgChan <- newChan

            _ <- watchDirChan
                    mgr
                    (root </> watchDir') --this is less wrong than earlier
                    (const True)
                    msgChan

            view <- case config of 
                    Right c -> do
                        return $ main app c msgChan conf stateFile state 

                    Left xxx -> 
                        return $ missingConf xxx

            startGUI
                defaultConfig { jsPort = Just port
                              , jsWindowReloadOnDisconnect = False
                              } (view root)
 


viewState :: FilePath -> FilePath -> ShakeConfig -> Window -> Chan String -> Chan String -> Chan String -> MVar States -> MVar ShakeConfig -> ListZipper State -> UI (Element, Element)
viewState root stateFile config w chan chanPhotographer chanSession states'' config'' states = do
    tmp <- UI.div
    case (focus states) of 


            Photographer -> do 
                c <- photographerSection root stateFile states'' states config config'' chanPhotographer
                return (c,tmp)

            DoneshootingBackup -> do
                c <- doneshootingBackupSection root stateFile states'' states config config''
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
                    dumpi <- liftIO $ withMVar config'' $ (\conf -> getDump conf)
                    withManager $ \mgr -> do
                            D.dump (return ()) --error "lol" -- return $ main c msgChan conf stateFile state 
                                    (\x -> do
                                            watchDirChan
                                                    mgr
                                                    x --this is less wrong than earlier
                                                    (const True)
                                                    dumpChan
                                            forever $ threadDelay 1000000
                                    )  dumpi
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

    
    photographer <- photographerOverview root stateFile config config'
    doneshootingBackup <- doneshootingBackupOverview root stateFile config config'

    session <- sessionOverview root stateFile config config' 
    shooting <- shootingOverview root stateFile config config' 
    location <- locationsOverview root stateFile config config'

    (buttonForward, forwardView) <- mkButton "next" "Ok"
    on UI.click buttonForward $ \_ -> liftIO $ do
            withMVar states' $ (\states ->  interpret $ setStates (mkFP root stateFile) states)

    view' <- mkSection [ element forwardView]

    view <- mkSection [ 
                       element doneshootingBackup
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
    

