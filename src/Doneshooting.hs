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
doneshootingSection config = 
    mkSection [ mkLabel "Doneshooting mappe"
              , readConf config
              , mkConfPicker config
              ]


readConf :: ShakeConfig -> UI Element
readConf config = do
    x <- liftIO $ getDoneshooting config
    UI.p # set UI.text (unDoneshooting x)


mkConfPicker :: ShakeConfig  -> UI Element
mkConfPicker config = do
    (_, view) <- mkFolderPicker "Vælg config folder" $ \folder -> do
        --that matter?
        liftIO $ setDoneshooting config $ Doneshooting { unDoneshooting = folder}
    return view
