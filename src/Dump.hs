{-# LANGUAGE OverloadedStrings #-}
module Dump
    ( dumpSection 
    ) where


import Elements
import System.FilePath


import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


dumpSection :: FilePath -> FilePath -> UI Element
dumpSection root dumpPath = mkSection [ mkLabel "Dump mappe" 
                                      , readConf root dumpPath 
                                      , mkConfPicker root dumpPath
                                      ]

--to delete
readConf :: FilePath -> FilePath -> UI Element
readConf root conf = do
    -- cant throw error
    x <- liftIO $ readFile (root </> conf)
    UI.p # set UI.text x

--readConf2 :: FilePath -> Shooting -> UI Element
--readConf2 _ x = do
--    UI.p # set UI.text (show x)


mkConfPicker :: FilePath -> FilePath -> UI Element
mkConfPicker root conf = do
    (_, view) <- mkFolderPicker "Vælg config folder" $ \folder -> do
        --this is full path will
        --that matter?
        writeFile (root </> conf) $ "location = " ++ folder
        return ()
    return view
