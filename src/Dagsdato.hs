{-# LANGUAGE OverloadedStrings #-}
module Dagsdato
    ( dagsdatoSection 
    ) where


import Elements
import PhotoShake.Dagsdato

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


import PhotoShake.ShakeConfig

dagsdatoSection :: ShakeConfig -> UI (Bool, Element)
dagsdatoSection  config =  do
    x <- liftIO $ getDagsdato config
    case x of
        NoDagsdato-> do
            gg <- mkSection [ mkLabel "Dagsdato mappe ikke valgt"
                            , mkConfPicker config
                            ]
            return (False, gg)
        Dagsdato y -> do
            gg <- mkSection [ mkLabel "Dagsdato mappe" 
                            , readConfig y
                            , mkConfPicker config
                            ]
            return (True, gg)


readConfig :: FilePath -> UI Element
readConfig x = do
    UI.p # set UI.text x


mkConfPicker :: ShakeConfig -> UI Element
mkConfPicker config = do
    (_, view) <- mkFolderPicker "Vælg config folder" $ \folder -> do
        liftIO $ setDagsdato config $ Dagsdato { unDagsdato = folder}
    return view

