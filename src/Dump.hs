{-# LANGUAGE OverloadedStrings #-}
module Dump
    ( dumpSection 
    ) where


import Elements

import PhotoShake.ShakeConfig
import PhotoShake.Dump

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core



dumpSection :: ShakeConfig -> UI (Bool, Element)
dumpSection config = do
    x <- liftIO $ getDump config
    case x of
        NoDump -> do
            gg <- mkSection [ mkLabel "Dump mappe ikke valgt"
                               , mkConfPicker config
                               ]
            return (False, gg)
        Dump y -> do
            gg <- mkSection [ mkLabel "Dump mappe" 
                               , readConf y
                               , mkConfPicker config
                               ]
            return (True, gg)
 
            
--to delete
readConf :: FilePath -> UI Element
readConf x = do
        UI.p # set UI.text x
    -- cant throw error


mkConfPicker :: ShakeConfig -> UI Element
mkConfPicker config = do
    (_, view) <- mkFolderPicker "dumpPicker" "Vælg config folder" $ \folder -> do
        liftIO $ setDump config $ Dump { unDump = folder }

    return view
