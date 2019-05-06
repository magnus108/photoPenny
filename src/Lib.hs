{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


import PhotoShake
import Photographee
import PhotoShake.ShakeConfig


import Control.Exception
import Development.Shake


---import System.FSNotify hiding (defaultConfig)
--import Control.Concurrent 


someFunc :: Int -> IO ()
someFunc port = do
    config <- try $ toShakeConfig "config.cfg" :: IO (Either SomeException ShakeConfig)
    let view = case config of 
            Right c -> setup c
            Left _ -> missingConf

    startGUI
        defaultConfig { jsStatic = Just "static"
                      , jsPort = Just port
                      } view
            

missingConf :: Window -> UI ()
missingConf w = do
    _ <- UI.addStyleSheet w "bulma.min.css"
    section <- mkSection [UI.p # set UI.text "Der skete en fejl"]
    _ <- getBody w #+ [element section] 
    return ()
    

funci :: ShakeConfig -> Window -> Element -> Element -> IO ()
funci config w err msg = do
    -- have to look this up from config
    let photographee = Photographee "test" "test" "12sads"
    build <- try $ myShake config photographee :: IO (Either ShakeException ())
    let ans = case build of
            Left _ -> element err # set text "Der skete en fejl"  
            Right _ -> element msg # set text "Byg færdigt"
    _ <- runUI w ans
    return ()


setup :: ShakeConfig -> Window -> UI ()
setup config w = do
    _ <- UI.addStyleSheet w "bulma.min.css"
    (button, view) <- mkButton "kør by"
    err <- UI.p
    msg <- UI.p

    callback <- ffiExport $ funci config w err msg
    let click = "click" :: String
    runFunction $ ffi "$(%1).on(%2,%3)" button click callback


    (button1, view1) <- mkButton "Åben config"

    on UI.click button1 $ \_ -> do 
        runFunction $ ffi "require('electron').shell.openItem('config.cfg')"

    section <- mkSection
            [ element view
            , element msg
            , element err
            , element view1
            ]

    _ <- getBody w #+ [element section] 

    return () 


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
