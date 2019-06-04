{-# LANGUAGE OverloadedStrings #-}
module Doneshooting
    ( doneshootingSection 
    ) where

import Elements
import PhotoShake.Doneshooting

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import PhotoShake.ShakeConfig



doneshootingSection :: ShakeConfig -> UI (Bool, Element)
doneshootingSection config = do
    x <- liftIO $ getDoneshooting config
    case x of
        NoDoneshooting -> do
            gg <- mkSection [ mkLabel "Doneshooting mappe ikke valgt"
                            , mkConfPicker config
                            ]
            return (False, gg)
        Doneshooting y -> do
            gg <- mkSection [ mkLabel "Doneshooting mappe" 
                            , readConf y
                            , mkConfPicker config
                            ]
            return (True, gg)
 
            
readConf :: FilePath -> UI Element
readConf x = do
    UI.p # set UI.text x


mkConfPicker :: ShakeConfig  -> UI Element
mkConfPicker config = do
    (_, view) <- mkFolderPicker "Vælg config folder" $ \folder -> do
        --that matter?
        liftIO $ setDoneshooting config $ Doneshooting { unDoneshooting = folder}
    return view
