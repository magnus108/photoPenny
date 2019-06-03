{-# LANGUAGE OverloadedStrings #-}
module Dagsdato
    ( dagsdatoSection 
    ) where


import Elements
import PhotoShake.Dagsdato

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


import PhotoShake.ShakeConfig

dagsdatoSection :: ShakeConfig -> UI Element
dagsdatoSection  config = mkSection [ mkLabel "Dagsdato mappe"
                                              , readConf config
                                              , mkConfPicker config
                                              ]
--
readConf :: ShakeConfig -> UI Element
readConf config = do
    -- cant throw error
    x <- liftIO $ getDagsdato config
    UI.p # set UI.text (unDagsdato x)


mkConfPicker :: ShakeConfig -> UI Element
mkConfPicker config = do
    (_, view) <- mkFolderPicker "Vælg config folder" $ \folder -> do
        liftIO $ setDagsdato config $ Dagsdato { unDagsdato = folder}
    return view

