{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( setup
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

--import Data.Time.Clock

import State
import Elements
--import PhotoShake.Photographer
--import PhotoShake.Session
--import PhotoShake.Doneshooting
--import PhotoShake.Shooting
--import PhotoShake.Photographee
--import PhotoShake.ShakeError
--import PhotoShake.Location
---import PhotoShake

--import PhotoShake.Dagsdato
--import PhotoShake.Dump

import PhotoShake.ShakeConfig

import System.FilePath

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


import Utils.Comonad
import Utils.ListZipper


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
                        return $ main c msgChan conf watchDir' stateFile state 
                    Left xxx -> 
                        return $ missingConf xxx

            startGUI
                defaultConfig { jsPort = Just port
                              , jsWindowReloadOnDisconnect = False
                              } (view root)
 


viewState :: FilePath -> FilePath -> ShakeConfig -> Window -> ListZipper State -> UI Element
viewState root stateFile config w states = do
    case (focus states) of 
            Dump -> dumpSection root stateFile states config

            Dagsdato -> dagsdatoSection root stateFile states config 

            Photographer -> photographerSection root stateFile states config

            Doneshooting -> doneshootingSection root stateFile states config

            Session -> sessionSection root stateFile states config

            Shooting -> shootingSection root stateFile states config

            Location -> locationsSection root stateFile states config

            Main -> mainSection root stateFile config w


redoLayout :: Window -> FilePath -> FilePath -> ShakeConfig -> States -> UI ()
redoLayout w root stateFile config (States states) = void $ do

    let views = states =>> viewState root stateFile config w
    view <- focus views
    let buttons = states =>> (\states' -> do
                        button <- UI.button # set (attr "id") ("tab" ++ show (focus states')) #. "button" #+ [string (show (focus states'))]
                        button' <- if (states' == states) then
                                set (UI.attr  "class") "button is-info is-selected" (element button)
                            else
                                return button

                        on UI.click button' $ \_ -> liftIO $ setStates root stateFile (States states')

                        return button'
                    )

    view'' <- mkSection [UI.div #. "buttons has-addons" #+ (toList buttons)]

    getBody w # set children [ view'', view]


recevier  :: Window -> FilePath -> FilePath -> FilePath -> FilePath -> EventChannel -> IO ()
recevier w root conf _ stateFile msgs = void $ do
    messages <- liftIO $ getChanContents msgs
    forM_ messages $ \_ -> do 
        -- actually also an try here :S
        state <- liftIO $ getStates root stateFile
        -- eww
        -- maybe i can hidaway errors on this one and deligate?
        config <- try $ toShakeConfig (Just root) conf :: IO (Either SomeException ShakeConfig)
        case config of 
            Right c ->
                runUI w $ redoLayout w root stateFile c state

            Left _ -> fail "ERROR"

-- eww
main :: ShakeConfig -> EventChannel -> FilePath -> FilePath -> FilePath -> States -> FilePath -> Window -> UI ()
main config msgChan conf watchDir' stateFile states root w = do
    _ <- addStyleSheet w root "bulma.min.css"

    msgs <- liftIO $ dupChan msgChan  

    redoLayout w root stateFile config states
    
    void $ liftIO $ forkIO $ recevier w root conf watchDir' stateFile msgs






    

--mkBuild :: ShakeConfig -> IORef String -> Window -> Element -> Element -> UI (Element, Element)
--mkBuild config idd w err msg = do
    --- with pattern
  --  (button, view) <- mkButton "mover" "Flyt filer"
    --callback <- ffiExport $ funci config idd w err msg
    --runFunction $ ffi "$(%1).on('click',%2)" button callback
   --return (button, view)


-- kinda of bad
missingConf :: SomeException -> FilePath -> Window -> UI ()
missingConf e root w = do
    _ <- addStyleSheet w root "bulma.min.css"
    section <- mkSection [UI.p # set UI.text ("Mangler måske config" ++ (show e))]
    _ <- getBody w #+ [element section] 
    return ()
    

