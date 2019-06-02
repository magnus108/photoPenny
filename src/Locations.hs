{-# LANGUAGE OverloadedStrings #-}
module Locations
    ( locationsSection 
    ) where

import Elements
import System.FilePath

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


locationsSection :: FilePath -> FilePath -> UI Element
locationsSection root locationFilePath = 
    mkSection [ mkLabel "Lokations Fil"
              , readConf root locationFilePath
              , mkConfPicker2 root locationFilePath
              ]


readConf :: FilePath -> FilePath -> UI Element
readConf root conf = do
    -- cant throw error
    x <- liftIO $ readFile (root </> conf)
    UI.p # set UI.text x

--readConf2 :: FilePath -> Shooting -> UI Element
--readConf2 _ x = do
--    UI.p # set UI.text (show x)


mkConfPicker2 :: FilePath -> FilePath -> UI Element
mkConfPicker2 root conf = do
    (_, view) <- mkFilePicker "Vælg config fil" $ \file -> do
        --this is full path will
        --that matter?
        writeFile (root </> conf) $ "location = " ++ file
        return ()
    return view
