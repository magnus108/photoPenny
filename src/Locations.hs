{-# LANGUAGE OverloadedStrings #-}
module Locations
    ( locationsSection 
    ) where

import Elements
import Prelude hiding (readFile, writeFile)
import Data.ByteString.Lazy (writeFile)

import Data.Csv

import PhotoShake.Photographee
import PhotoShake.ShakeConfig
import PhotoShake.Location
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (empty)

import Utils.ListZipper
import State (State, States(..), setStates)

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue

locationsSection :: FilePath -> FilePath -> TBMQueue States -> ListZipper State -> ShakeConfig -> UI Element
locationsSection root stateFile queue states config = do

    x <- liftIO $ getLocationFile config

    (_, view) <- mkFilePicker "locationsPicker" "Vælg eksisterende CSV fil" $ \file -> do
        liftIO $ setLocation config $ Location file

    (_, view2) <- mkFileMaker "locationsPicker" "Ny CSV" $ \file -> do
        let empty = mempty :: [Photographee]
        _ <- liftIO $ writeFile file (encode empty)

        liftIO $ setLocation config $ Location file

    case x of
        NoLocation -> 
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Lokations fil ikke valgt" ]
                            , mkColumn ["is-12"] [ UI.div #. "field is-grouped" #+ [element view, element view2]]
                            ]
                      ] 

        Location y -> do
            (buttonForward, forwardView) <- mkButton "nextDump" "Ok"
            on UI.click buttonForward $ \_ -> liftIO $ setStates queue (States (forward states))
            
            (buttonOpen, openView) <- mkButton "open" "Åben csv"
            on UI.click buttonOpen $ \_ -> do 
                    runFunction $ ffi $ "require('electron').shell.openItem('" ++ y ++ "')"

            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Lokations mappe" # set (attr "id") "locationOK" ]
                            , mkColumn ["is-12"] [ UI.div #. "field is-grouped" #+ [element view, element view2]]
                            , mkColumn ["is-12"] [ UI.p # set UI.text y]
                            , mkColumn ["is-12"] [ element openView ]
                            , mkColumn ["is-12"] [ element forwardView ]
                            ]
                      ] 


