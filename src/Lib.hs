{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Debug.Trace

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

import Language.Javascript.JMacro

someFunc :: Int -> IO ()
someFunc port = do
    config <- try $ toShakeConfig "config.cfg" :: IO (Either SomeException ShakeConfig)
    msgChan <- newChan
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
                              } view
            

missingConf :: Window -> UI ()
missingConf w = do
    _ <- UI.loadFile "text/css" "static"
    _ <- UI.addStyleSheet w "bulma.min.css"
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

gg :: UI ()
gg = runFunction $ ffi $ show $ renderJs
        [jmacro|
            var remote = require('electron').remote;
            var path = remote.require('path');
            var {|BrowserWindow: BrowserWindow|} = remote;
        |]

addStyleSheet :: Window -> FilePath -> UI ()
addStyleSheet w filename = void $ do
    el <- mkElement "link"
            # set (attr "rel" ) "stylesheet"
            # set (attr "type") "text/css"
            # set (attr "href") filename
    getHead w #+ [element el]


setup :: (IORef ShakeConfig) -> EventChannel -> Window -> UI ()
setup config msgChan w = do
    x <- UI.loadFile "text/css" "static/css/bulma.min.css"
    traceShowM x
    _ <- addStyleSheet w x

    _ <- gg

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


mkSection :: [UI Element] -> UI Element
mkSection xs =
    UI.div #. "section" #+ 
        [UI.div #. "container is-fluid" #+ xs]


mkButton :: String -> UI (Element, Element)
mkButton x = do
    button <- UI.button #. "button" #+ [string x]
    view <- UI.div #. "control" #+ [element button]
    return (button, view)


--mkLabel :: String -> UI Element
--mkLabel s =
--    UI.p #. "has-text-info has-text-weight-bold is-size-5" # set UI.text s
