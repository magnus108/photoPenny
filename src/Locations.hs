{-# LANGUAGE OverloadedStrings #-}
module Locations
    ( locationsSection 
    ) where

import Elements

import PhotoShake.ShakeConfig
import PhotoShake.Location
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


locationsSection :: ShakeConfig -> UI Element
locationsSection config = do

    x <- liftIO $ getLocationFile config

    (_, view) <- mkFilePicker "locationsPicker" "Vælg config fil" $ \file -> do
        liftIO $ setLocation config $ Location file

    case x of
        NoLocation -> do
            mkSection [ mkLabel "Lokations fil ikke valgt"
                      , element view
                      ]

        Location y -> 
            mkSection [ mkLabel "Lokations mappe" 
                      , UI.p # set UI.text y
                      , element view
                      ]


