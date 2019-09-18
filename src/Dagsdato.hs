{-# LANGUAGE OverloadedStrings #-}
module Dagsdato
    ( dagsdatoSection
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import qualified Message as Msg

import Control.Monad 

import Elements

import PhotoShake.Dagsdato

dagsdatoSection :: Chan Msg.Message -> Dagsdato -> UI Element
dagsdatoSection msgs x = do
    (_, picker) <- mkFolderPicker "doneshootingPicker" "Vælg config folder" $ \folder -> when (folder /= "") $ do
        liftIO $ Chan.writeChan msgs $ Msg.setDagsdato $ yesDagsdato folder

    dagsdato ( mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dagsdato mappe ikke valgt" # set (attr "id") "dagsdatoMissing" ]
                            , mkColumn ["is-12"] [ element picker ]
                            ]
                      ] )
                (\y -> do
                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Dagsdato mappe" # set (attr "id") "dagsdatoOK" ]
                                    , mkColumn ["is-12"] [ element picker ]
                                    , mkColumn ["is-12"] [ UI.p # set UI.text y # set (attr "id") "dagsdatoPath" ]
                                    ]
                              ] 
                ) x
