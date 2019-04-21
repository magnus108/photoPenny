{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import qualified PhotoShake as PS

import Control.Monad
import Control.Exception

import System.Exit
import System.Process

import Debug.Trace


mkButton :: String -> String -> UI (Element, Element)
mkButton title id = do
    button <- UI.button #. "button" # set UI.id_ id #+ [string title]
    view <- UI.p #+ [element button]
    return (button, view)


setup :: Window -> UI ()
setup w = do
    (button, view) <- mkButton "Run build" "thisId"

    input <- 
        UI.div #. "column is-one-third" #+
            [ UI.div #. "field" #+ 
                [ UI.label #. "label" # set text "Path"
                , UI.div #. "control" #+ [ UI.input #. "input" # set UI.type_ "text" ]
                ]
            ]

    msg <- UI.span # set UI.text "Some text"

    wrap <- UI.div #. "container" #. "is-fluid" #+ [element view, element msg, UI.div #. "columns" #+ [element input]]
    section <- UI.div #. "section" #+ [element wrap]

    getBody w #+ [element section]
    
     
    onElementId "thisId" "click"
        (element msg # set text "Clicked")
        msg


onElementId :: String -> String -> UI void -> Element -> UI ()
onElementId elid event handler err = do
    window   <- askWindow
    exported <- ffiExport $ do
        --(exitcode, stdout, stderr) <- myProcess
        lol <- PS.someFunc2 "config.cfg"
        case lol of
            Left x -> runUI window (element err # set text (displayException x)) >> return ()
            Right _ -> runUI window handler >> return ()
        --someFunc2 :: FilePath -> IO (Either SomeException (IO ()))
    runFunction $ ffi "$(%1).on(%2,%3)" ("#"++elid) event exported


--myProcess :: IO (ExitCode, String, String)
--myProcess = 
--    readCreateProcessWithExitCode 
--        ((shell "photoShake-exe") 
--            {cwd = Just "/home/magnus/Documents/projects/photoShake/" }) ""


someFunc :: Int -> IO ()
someFunc port =
    startGUI
        defaultConfig { jsCustomHTML = Just "index.html"
                      , jsStatic = Just "static"
                      , jsPort = Just port
                      } 
        setup
