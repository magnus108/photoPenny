{-# LANGUAGE OverloadedStrings #-}
module Doneshooting
    ( doneshootingSection 
    ) where

import Elements

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


doneshootingSection :: FilePath -> FilePath -> UI Element
doneshootingSection root doneshootingPath = 
    mkSection [ mkLabel "Doneshooting mappe"
              , readConf root doneshootingPath
              , mkConfPicker2 root doneshootingPath
              ]


readConf :: FilePath -> FilePath -> UI Element
readConf _ conf = do
    -- cant throw error
    x <- liftIO $ readFile conf
    UI.p # set UI.text x

--readConf2 :: FilePath -> Shooting -> UI Element
--readConf2 _ x = do
--    UI.p # set UI.text (show x)


mkConfPicker2 :: FilePath -> FilePath -> UI Element
mkConfPicker2 _ conf = do
    (_, view) <- mkFilePicker "Vælg config fil" $ \file -> do
        --this is full path will
        --that matter?
        writeFile conf $ "location = " ++ file
        return ()
    return view
