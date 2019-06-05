{-# LANGUAGE OverloadedStrings #-}
module Locations
    ( locationsSection 
    ) where

import Elements

import PhotoShake.ShakeConfig
import PhotoShake.Location
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


locationsSection :: ShakeConfig -> UI (Bool, Element)
locationsSection config = do
    x <- liftIO $ getLocationFile config
    case x of
        NoLocation -> do
            gg <- mkSection [ mkLabel "Lokations fil ikke valgt"
                            , mkConfPicker2 config
                            ]
            return (False, gg)
        Location y -> do
            gg <- mkSection [ mkLabel "Lokations mappe" 
                            , readConf y
                            , mkConfPicker2 config
                            ]
            return (True, gg)


readConf :: FilePath -> UI Element
readConf x = do
    UI.p # set UI.text x



mkConfPicker2 :: ShakeConfig -> UI Element
mkConfPicker2 config = do
    (_, view) <- mkFilePicker "locationsPicker" "Vælg config fil" $ \file -> do
        liftIO $ setLocation config $ Location { unLocation = file}
    return view
