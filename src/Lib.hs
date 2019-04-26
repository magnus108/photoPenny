{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import System.Directory

import qualified PhotoShake as PS

import Development.Shake

import Control.Monad
import Control.Exception

import System.Exit
import System.Process

import Data.HashMap.Lazy
import Development.Shake.FilePath

import System.FSNotify hiding (defaultConfig)
import Control.Concurrent (newChan, readChan, dupChan, forkIO, getChanContents)
import Control.Monad (forever)

import Debug.Trace

someFunc :: Int -> IO ()
someFunc port = do
    config <- PS.readConfigFile "config.cfg"
    dirs <- listDirectory $ PS._dumpDir config
    withManager $ \mgr -> do
        -- start a watching job (in the background)
        outChan <- newChan
        dumpChan <- newChan
        watchDirChan
            mgr
            "/home/magnus/Documents/projects/photoPenny/out"
            (const True)
            outChan

        watchDirChan
            mgr          -- manager
            "/home/magnus/Documents/projects/photoPenny/dump"
            (const True) -- predicate
            dumpChan

        startGUI
            defaultConfig { jsCustomHTML = Just "index.html"
                      , jsStatic = Just "static"
                      , jsPort = Just port
                      } 
            (setup config dirs dumpChan)

mkButton :: String -> String -> UI (Element, Element)
mkButton title id = do
    button <- UI.button #. "button" # set UI.id_ id #+ [string title]
    view <- UI.div #. "control" #+ [element button]
    return (button, view)


mkSection :: [UI Element] -> UI Element
mkSection xs =
    UI.div #. "section" #+ 
        [UI.div #. "container is-fluid" #+ xs]

mkLabel :: String -> UI Element
mkLabel s =
    UI.p #. "has-text-info has-text-weight-bold is-size-5" # set UI.text s


setup :: PS.Config -> [FilePath] -> EventChannel -> Window -> UI ()
setup config dumps dumpChan w = do
    (button, view) <- mkButton "Run build" "thisId"
    (button1, view1) <- mkButton "Select folder" "thisId1"
 
    dumpChanges <- UI.p

    dump <- mkSection
        [ mkLabel "dumpDir"
        , UI.p # set UI.text (PS._dumpDir config)
        , UI.p # set UI.text (head dumps)
        , element dumpChanges
        ]

    out <- mkSection
        [ mkLabel "outDir"
        , UI.p # set UI.text (PS._outDir config)
        ]

    location <- mkSection
        [ mkLabel "location"
        , UI.p # set UI.text (PS._location config)
        ]

    photoConfig <- mkSection 
        [ mkLabel "photographeeId"
        , UI.p # set UI.text "not sat"
        ]

    section <- mkSection [element view1, element view
            , element dump
            , element out
            , element location
            , element photoConfig
            ]

    getBody w #+ [element section] 
         
   -- onElementId "thisId" "click"
    --    (element msg # set text "Clicked")
     --   msg
     --
    --selectFolder "thisId1" "click" $ \x -> do
    --    putStrLn "lol"
    --    return ()
    --on UI.click button1 $ \_ -> do
    --    runFunction $ ffi "require('electron').remote.dialog.showOpenDialog({properties: ['openDirectory']}, (folder) => { console.log(folder)})"
    
    on UI.click button1 $ \_ -> do
        callback <- ffiExport $ \folder -> do
            putStrLn folder
            return ()

        runFunction $ ffi "require('electron').remote.dialog.showOpenDialog({properties: ['openDirectory']}, %1)" callback

    dumpChan' <- liftIO $ dupChan dumpChan

    void $ liftIO $ forkIO $ receiveDumps w dumpChan' dumpChanges
    --
    --on UI.click button $ \_ -> do 
      --  runFunction $ ffi "require('electron').shell.openItem('/home/magnus/Downloads/fetmule.jpg')"


receiveDumps :: Window -> EventChannel -> Element -> IO ()
receiveDumps w events dumpFiles = do
    messages <- getChanContents events
    forM_ messages $ \msg -> do
        runUI w $ do
          element dumpFiles #+ [UI.p # set UI.text (show msg)]
          flushCallBuffer


selectFolder :: String -> String -> (FilePath -> IO ()) -> UI ()
selectFolder elid event complete = do
    callback <- ffiExport complete
    runFunction $ ffi "$(%1).on(%2,require('electron').remote.dialog.showOpenDialog({properties: ['openDirectory']}, %3))" ("#"++elid) event callback


onElementId :: String -> String -> UI void -> Element -> UI ()
onElementId elid event handler err = do
    window   <- askWindow
    exported <- ffiExport $ do
        lol <- try PS.someFunc :: IO (Either ShakeException ())

        case lol of
            Left x -> runUI window (element err # set text (displayException (shakeExceptionInner x))) >> return ()
            Right _ -> runUI window handler >> return ()
   
    runFunction $ ffi "$(%1).on(%2,%3)" ("#"++elid) event exported


