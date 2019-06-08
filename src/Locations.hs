{-# LANGUAGE OverloadedStrings #-}
module Locations
    ( locationsSection 
    ) where

import Elements

import PhotoShake.ShakeConfig
import PhotoShake.Location
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Utils.Comonad
import Utils.ListZipper
import State (State, States(..), setStates)


locationsSection :: FilePath -> ListZipper State -> ShakeConfig -> UI Element
locationsSection root states config = do

    x <- liftIO $ getLocationFile config

    (_, view) <- mkFilePicker "locationsPicker" "Vælg config fil" $ \file -> do
        liftIO $ setLocation config $ Location file

    case x of
        NoLocation -> 
            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Lokations fil ikke valgt" ]
                            , mkColumn ["is-12"] [ element view ]
                            ]
                      ] 

        Location y -> do
            (buttonForward, forwardView) <- mkButton "nextDump" "Ok"
            on UI.click buttonForward $ \_ -> liftIO $ setStates root (States (forward states))

            mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Lokations mappe" ]
                            , mkColumn ["is-12"] [UI.p # set UI.text y]
                            , mkColumn ["is-12"] [ element forwardView ]
                            ]
                      ] 


