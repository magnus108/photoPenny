{-# LANGUAGE OverloadedStrings #-}
module Dump
    ( dumpSection 
    ) where

import Prelude hiding (writeFile)
import Data.ByteString.Lazy hiding (take, putStrLn)

import Elements
import System.FilePath

import PhotoShake.ShakeConfig
import PhotoShake.Dump

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import PhotoShake.Shooting


dumpSection :: FilePath -> FilePath -> UI Element
dumpSection root dumpPath = mkSection [ mkLabel "Dump mappe" 
                                      , readConf root dumpPath 
                                      , mkConfPicker root dumpPath
                                      ]

--to delete
readConf :: FilePath -> FilePath -> UI Element
readConf root conf = do
    -- cant throw error
    x <- liftIO $ getDump (root </> conf)
    UI.p # set UI.text (unDump x)

--readConf2 :: FilePath -> Shooting -> UI Element
--readConf2 _ x = do
--    UI.p # set UI.text (show x)


mkConfPicker :: FilePath -> FilePath -> UI Element
mkConfPicker root config = do
    (_, view) <- mkFolderPicker "Vælg config folder" $ \folder -> do
        --this is full path will
        --that matter?
        liftIO $ writeFile (root </> config) $ encode (Dump { unDump = folder })
        return ()
    return view
