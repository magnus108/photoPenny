{-# LANGUAGE OverloadedStrings #-}
module Summary
    ( summarySection 
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import PhotoShake
import PhotoShake.ShakeConfig

import Elements

summarySection :: FilePath -> FilePath -> ShakeConfig -> Window -> UI Element
summarySection _ _ _ _ = do
    UI.div #+ [UI.string "loL"]
