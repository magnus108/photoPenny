{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


import PhotoShake
import Photographee
import PhotoShake.ShakeConfig

import Development.Shake.FilePath

import Control.Monad 

import Control.Exception
import Development.Shake

import System.FSNotify hiding (defaultConfig)
import Control.Concurrent
import Data.IORef

import Elements

import Debug.Trace


someFunc :: Int -> String -> IO ()
someFunc port root = do
    -- opret config hvis den ikke findes
    config <- try $ toShakeConfig "config.cfg" :: IO (Either SomeException ShakeConfig)
    msgChan <- newChan
    traceShowM root
    traceShowM port
    withManager $ \mgr -> do
            _ <- watchDirChan
                    mgr
                    "."
                   (\x -> takeFileName (eventPath x) == "config.cfg") -- need a better check
                    msgChan

            view <- case config of 
                    Right c -> do
                        conf <- newIORef c
                        return $ setup conf msgChan
                    Left _ -> return missingConf

            startGUI
                defaultConfig { jsPort = Just port
                              } (view root)
            


missingConf :: FilePath -> Window -> UI ()
missingConf root w = do
    _ <- addStyleSheet w root "bulma.min.css"
    section <- mkSection [UI.p # set UI.text "Mangler måske config"]
    _ <- getBody w #+ [element section] 
    return ()
    

funci :: (IORef ShakeConfig) -> (IORef String) -> Window -> Element -> Element -> IO ()
funci config idd w err msg = do
    -- have to look this up from config
    idd2 <- readIORef idd

    conf <- readIORef config

    let photographee = Photographee "test" "test" idd2
    build <- try $ myShake conf photographee :: IO (Either ShakeException ())
    let ans = case build of
            Left _ -> element err # set text "Der skete en fejl"  
            Right _ -> element msg # set text "Byg færdigt"
    _ <- runUI w ans
    return ()


setup :: (IORef ShakeConfig) -> EventChannel -> String -> Window -> UI ()
setup config msgChan root w = do
    _ <- addStyleSheet w root "bulma.min.css"

    (button, view) <- mkButton "kør by"
    err <- UI.p
    msg <- UI.p

    input <- UI.input 
    idd <- liftIO $ newIORef ""
    on UI.keyup input $ \_ -> liftIO . writeIORef idd =<< get value input

    callback <- ffiExport $ funci config idd w err msg
    let click = "click" :: String
    runFunction $ ffi "$(%1).on(%2,%3)" button click callback


    (button1, view1) <- mkButton "Åben config"

    on UI.click button1 $ \_ -> do 
        runFunction $ ffi "require('electron').shell.openItem('config.cfg')"


    msgChanges <- UI.p
    msgChan' <- liftIO $ dupChan msgChan
    void $ liftIO $ forkIO $ receiveMsg w msgChan' config msgChanges


    section <- mkSection
            [ element view
            , element msg
            , element err
            , element view1
            , element input
            , element msgChanges
            ]

    _ <- getBody w #+ [element section] 

    return () 


receiveMsg :: Window -> EventChannel -> (IORef ShakeConfig) -> Element -> IO ()
receiveMsg w events config ident = do
    messages <- getChanContents events
    forM_ messages $ \msg -> do 
        -- handle more gracefully pls
        config' <- try $ toShakeConfig "config.cfg" :: IO (Either SomeException ShakeConfig)
        _ <- case config' of 
                Right c -> modifyIORef config (\_ -> c)
                Left _ -> fail "ERROR"

        runUI w $ do
          _ <- element ident # set UI.text (show msg)
          flushCallBuffer
