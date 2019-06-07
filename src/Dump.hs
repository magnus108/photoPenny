{-# LANGUAGE OverloadedStrings #-}
module Dump
    ( dumpSection 
    ) where


import Elements

import PhotoShake.ShakeConfig
import PhotoShake.Dump

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


dumpSection :: ShakeConfig -> UI Element
dumpSection config = do
    x <- liftIO $ getDump config

    (_, picker) <- mkFolderPicker "dumpPicker" "Vælg config folder" $ \folder ->
            liftIO $ setDump config $ Dump folder

    case x of
        NoDump -> 
            mkSection [ mkLabel "Dump mappe ikke valgt"
                      , element picker
                      ]

        Dump y ->
            mkSection [ mkLabel "Dump mappe" 
                      , UI.p # set UI.text y
                      , element picker
                      ] 
