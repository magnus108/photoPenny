{-# LANGUAGE OverloadedStrings #-}
module Doneshooting
    ( doneshootingSection 
    ) where

import Elements
import PhotoShake.Doneshooting

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import PhotoShake.ShakeConfig



doneshootingSection :: ShakeConfig -> UI Element
doneshootingSection config = do
    x <- liftIO $ getDoneshooting config

    (_, view) <- mkFolderPicker "doneshotingPicker" "Vælg config folder" $ \folder -> do
            liftIO $ setDoneshooting config $ Doneshooting { unDoneshooting = folder}

    case x of
        NoDoneshooting -> do

            mkSection [ mkLabel "Doneshooting mappe ikke valgt"
                      , element view
                      ]

        Doneshooting y -> do
            mkSection [ mkLabel "Doneshooting mappe" 
                      , UI.p # set UI.text y
                      , element view
                      ]
