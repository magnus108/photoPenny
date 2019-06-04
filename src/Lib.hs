{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( setup
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements
import PhotoShake.Photographer
import PhotoShake.Session
import PhotoShake.Doneshooting
import PhotoShake.Shooting
import PhotoShake.Photographee
import PhotoShake.ShakeError
import PhotoShake.Location
import PhotoShake

import PhotoShake.Dagsdato
import PhotoShake.Dump

import PhotoShake.ShakeConfig

import System.FilePath

import Photographer
import Session
import Shooting
import Dump
import Dagsdato
import Doneshooting
import Locations

import Control.Monad 

import Control.Exception

import System.FSNotify hiding (defaultConfig)
import Control.Concurrent
import Data.IORef




setup :: Int -> String -> IO ()
setup port root = do
    config <- try $ toShakeConfig (Just root) "config.cfg" :: IO (Either SomeException ShakeConfig)
    withManager $ \mgr -> do
            msgChan <- newChan
            _ <- watchDirChan
                    mgr
                    (root </> "config") --this is kind of wrong
                    (const True)
                    msgChan

            view <- case config of 
                    Right c -> do
                        conf <- newIORef c
                        return $ main conf msgChan
                    Left xxx -> return $ missingConf xxx

            startGUI
                defaultConfig { jsPort = Just port
                              } (view root)

            
receiveMsg :: Window -> FilePath -> IORef ShakeConfig -> EventChannel -> IO ()
receiveMsg w root config events = do
    messages <- getChanContents events
    forM_ messages $ \_ -> do 
        -- handle more gracefully pls
        config' <- try $ toShakeConfig (Just root) "config.cfg" :: IO (Either SomeException ShakeConfig)
        _ <- case config' of 
                Right c -> modifyIORef config (\_ -> c)
                Left _ -> fail "ERROR"
        --nicess
        runUI w (body w root config events)


main :: IORef ShakeConfig -> EventChannel -> FilePath -> Window -> UI ()
main shakeConfig msgChan root w = do
    _ <- addStyleSheet w root "bulma.min.css"
    _ <- body w root shakeConfig msgChan
    return ()



body :: Window -> FilePath -> IORef ShakeConfig -> EventChannel -> UI ()
body w root config msgChan = do
    
    conf <- liftIO $ readIORef config 



    err <- UI.p 
    msg <- UI.p 
    ident <- liftIO $ newIORef ""
    (_, buildView) <- mkBuild conf ident w err msg

    (input, inputView) <- mkInput "Elev nr:"
    on UI.keyup input $ \_ -> liftIO . writeIORef ident =<< get value input

    inputView2 <- mkSection $ 
                    [ mkColumns ["is-multiline"]
                        [ mkColumn ["is-4"] [element inputView]
                        , mkColumn ["is-12"] [element buildView]
                        , mkColumn ["is-12"] [element err, element msg] 
                        ]
                    ]
    (b1, dump) <- dumpSection conf
    (b2, dagsdato) <- dagsdatoSection conf
    (b3, doneshooting) <- doneshootingSection conf
    (b4, photographer) <- photographerSection conf

    (b5, shooting) <- shootingSection conf
    (b6, session) <- sessionSection conf

    (b7, location) <- locationsSection conf


    (_, viewReset)<- mkReset conf w 
    viewReset2 <- mkSection $ 
                    [ mkColumns ["is-multiline"]
                        [ mkColumn ["is-4"] [element viewReset] ]
                    ]

    _ <- if (not b1) then 
        getBody w # set children [dump] 
    else if (not b2) then
        getBody w # set children [dagsdato]
    else if (not b3) then
        getBody w # set children [doneshooting]
    else if (not b4) then
        getBody w # set children [photographer]
    else if (not b5) then
        getBody w # set children [shooting]
    else if (not b6) then
        getBody w # set children [session]
    else if (not b7) then
        getBody w # set children [location]
    else
        getBody w # set children [viewReset2, inputView2, 
                    photographer, dump, dagsdato
                    , doneshooting, shooting, session, location]


    msgChan' <- liftIO $ dupChan msgChan
    void $ liftIO $ forkIO $ receiveMsg w root config msgChan'
    
    return ()


mkReset :: ShakeConfig -> Window -> UI (Element, Element)
mkReset config w = do
    (button, view) <- mkButton "Reset konfiguration"
    callback <- ffiExport $ resetIt config w
    runFunction $ ffi "$(%1).on('click',%2)" button callback
    return (button, view)

resetIt :: ShakeConfig -> Window -> IO ()
resetIt config _ = 
        setDump config NoDump
        >> setDagsdato config NoDagsdato
        >> setPhotographers config NoPhotographers
        >> setLocation config NoLocation
        >> setSession config NoSessions
        >> setShooting config NoShootings
        >> setDoneshooting config NoDoneshooting

mkBuild :: ShakeConfig -> IORef String -> Window -> Element -> Element -> UI (Element, Element)
mkBuild config idd w err msg = do
    (button, view) <- mkButton "Flyt filer"
    callback <- ffiExport $ funci config idd w err msg
    runFunction $ ffi "$(%1).on('click',%2)" button callback
    return (button, view)


-- kinda of bad
missingConf :: SomeException -> FilePath -> Window -> UI ()
missingConf e root w = do
    _ <- addStyleSheet w root "bulma.min.css"
    section <- mkSection [UI.p # set UI.text ("Mangler måske config" ++ (show e))]
    _ <- getBody w #+ [element section] 
    return ()
    

funci :: ShakeConfig -> (IORef String) -> Window -> Element -> Element -> IO ()
funci config idd w err msg = do
    --have to look this up from config
    idd2 <- readIORef idd
    locationFile <- getLocationFile config
    -- kinda bad here
    -- kinda bad here
    -- kinda bad here
    -- kinda bad here
    -- kinda bad here
    -- kinda bad here
    -- kinda bad here
    -- kinda bad here could cause errorr
    find <- try $ findPhotographee (unLocation locationFile) idd2 :: IO (Either ShakeError Photographee)

    case find of
            Left errMsg -> do
                    _ <- runUI w $ element err # set text (show errMsg)
                    return ()

            Right photographee -> do
                    build <- try $ myShake config photographee (takeBaseName (unLocation locationFile)) :: IO (Either ShakeError ())
                    let ans = case build of
                            Left errMsg -> element err # set text (show errMsg)
                            Right _ -> element msg # set text "Byg færdigt"
                    -- reset
                    _ <- runUI w (element err # set text "")
                    _ <- runUI w (element msg # set text "")
                    _ <- runUI w ans
                    return ()
