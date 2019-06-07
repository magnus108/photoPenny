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
dagsdatoSection  config =  do
    x <- liftIO $ getDagsdato config

    (_, view) <- mkFolderPicker "dagsDatoPicker" "Vælg config folder" $ \folder ->
        liftIO $ setDagsdato config $ Dagsdato { unDagsdato = folder}

    case x of
        NoDagsdato-> do
            mkSection [ mkLabel "Dagsdato mappe ikke valgt"
                      , element view
                      ]

        Dagsdato y ->
            mkSection [ mkLabel "Dagsdato mappe" 
                      , UI.p # set UI.text y
                      , element view
                      ]
