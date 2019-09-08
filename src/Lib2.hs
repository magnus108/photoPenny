{-# LANGUAGE OverloadedStrings #-}
module Lib2
    ( main
    ) where

import Control.Concurrent.MVar

import qualified State as S
import Utils.ListZipper
import Utils.FP
import Utils.Comonad
import Utils.Actions


import Model.E
import qualified Message as Msg

import Elements

import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Monad 

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import System.FSNotify hiding (defaultConfig)
import System.FilePath hiding (combine)


import Data.Function ((&))


import Control.Exception
import PhotoShake.ShakeConfig
import PhotoShake.Dump


main :: Int -> Chan Msg.Message -> MVar (App Model) -> IO ()
main port messages app = do 
    manager <- startManager
    startGUI defaultConfig { jsPort = Just port
                           , jsWindowReloadOnDisconnect = False
                           } $ setup manager messages app



setupStateListener :: WatchManager -> Chan Msg.Message -> MVar (App Model) -> IO StopListening --- ??? STOP
setupStateListener manager msgChan app = do -- THIS BAD
    fpConfig <- withMVar app (\app' -> return (_configs app'))
    stateConfig <- withMVar app (\app' -> return (_stateFile app'))
    watchDir manager fpConfig 
        (\e -> takeFileName (eventPath e) == takeFileName stateConfig) (\_ -> writeChan msgChan Msg.getStates)


setupDumpListener :: WatchManager -> Chan Msg.Message -> MVar (App Model) -> IO StopListening --- ??? STOP
setupDumpListener manager msgChan app = do -- THIS BAD
    fpConfig <- withMVar app (\app' -> return (_configs app'))
    dumpConfig <- withMVar app (\app' -> return (_dumpFile app'))
    watchDir manager fpConfig 
        -- THIS IS LIE
        (\e -> takeFileName (eventPath e) == takeFileName dumpConfig) (\_ -> writeChan msgChan Msg.getStates)




getStates :: Chan Msg.Message -> UI ()
getStates msgChan = liftIO $ writeChan msgChan Msg.getStates

setStates :: Chan Msg.Message -> S.States -> UI ()
setStates msgChan states = liftIO $ writeChan msgChan (Msg.setStates states) 


setup :: WatchManager -> Chan Msg.Message -> MVar (App Model) -> Window -> UI ()
setup manager msgChan app w = do
    msgs <- liftIO $ Chan.dupChan msgChan
    -- do i really watch all files in setup?
    _ <- liftIO $ setupStateListener manager msgs app
    _ <- liftIO $ setupDumpListener manager msgs app

    _ <- getStates msgs 

    receiver <- liftIO $ forkIO $ receive w msgs app

    on UI.disconnect w $ const $ liftIO $ killThread receiver
    on UI.disconnect w $ const $ liftIO $ stopManager manager


--mountCmd :: Chan Msg.Message -> S.States -> IO ()
-- Thesemessage does not use liftIO
--S.Dump -> writeChan msgChan
-- maybe i dont have to write
mountCmd :: Chan Msg.Message -> App Model ->  IO (App Model)
mountCmd msgs app = do
    case _states app of
        Nothing -> return app
        Just (S.States states) ->  
            case focus states of
                S.Dump -> do
                    let shakeConfig = _shakeConfig app
                    dump <- getDump shakeConfig
                    let app' = _setDump app dump
                    return app'
                    
                _ -> return app


receive :: Window -> Chan Msg.Message -> MVar (App Model) -> IO ()
receive w msgs app = do
    messages <- getChanContents msgs
    
    forM_ messages $ \ msg -> do 
        case msg of
            Msg.GetStates -> do                
                app' <- takeMVar app 
                let root = _root app'
                let stateFile = _stateFile app'
                states <- interpret $ S.getStates $ fp $ unFP root =>> combine stateFile
                let app'' = _setStates app' (Just states)

                app''' <- mountCmd msgs app''

                runUI w $ do
                    _ <- addStyleSheet w "" "bulma.min.css" --delete me
                    body <- getBody w
                    redoLayout body msgs app'''

                putMVar app app'''

            Msg.SetStates states -> do
                app' <- takeMVar app 
                let root = _root app'
                let stateFile = _stateFile app'
                interpret $ S.setStates (fp (unFP root =>> combine stateFile)) states
                let app'' = _setStates app' Nothing -- i dont think i have to do this
                _ <- runUI w $ do
                    body <- getBody w
                    redoLayout body msgs app''
                putMVar app app'
    
            Msg.Block x -> do -- i can maybe do something good with this.
                putMVar x () 


redoLayout :: Element -> Chan Msg.Message -> App Model -> UI ()
redoLayout body msgs app = void $ do
    
    let states = _states app

    case states of
        Just (S.States states) -> do
            let buttons = states =>> (\ states' -> do
                                button <- UI.button # set (attr "id") ("tab" ++ show (focus states')) #. "button" #+ [string (show (focus states'))]

                                button' <- if (states' == states) then
                                    set (UI.attr  "class") "button is-dark is-selected" (element button)
                                else
                                    return button

                                on UI.click button' $ \_ -> do
                                    setStates msgs (S.States states')

                                return button')

            menu <- mkSection [UI.div #. "buttons has-addons" #+ (toList buttons)]

            view <- states =>> viewState msgs app & focus

            element body # set children [menu, view]
                 

        Nothing -> do
            element body # set children []


--viewState :: Element -> Chan Msg.Message -> App Model > UI (Element, Element)
viewState :: Chan Msg.Message -> App Model -> (ListZipper S.State) -> UI Element
viewState msgs app states = do
    case (focus states) of 
            S.Dump -> dumpSection msgs app
            _ -> do
                string "bob"


dumpSection :: Chan Msg.Message -> App Model -> UI Element
dumpSection msgs app = do
    let dump = _dump app
    --(_, picker) <- mkFolderPicker "dumpPicker" "Vælg config folder" $ \folder ->
    --        liftIO $ withMVar config' $ (\conf -> setDump conf $ Dump folder)


    case dump of
        NoDump -> 
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dump mappe ikke valgt" ]
            --                , mkColumn ["is-12"] [ element picker ]
                            ]
                      ] 

        Dump y -> do
            ---(buttonForward, forwardView) <- mkButton "nextDump" "Ok"
            --on UI.click buttonForward $ \_ -> liftIO $ withMVar states'' $ (\_ ->  interpret $ setStates (mkFP root stateFile) (States (forward states)))

            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dump mappe" # set (attr "id") "dumpOK" ]
                            --, mkColumn ["is-12"] [ element picker ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y ]
                            --, mkColumn ["is-12"] [ element forwardView ]
                            ]
                      ] 



















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
