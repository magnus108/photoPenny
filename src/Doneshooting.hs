{-# LANGUAGE OverloadedStrings #-}
module Doneshooting
    ( doneshootingSection
    ) where

import Control.Monad 

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import qualified Message as Msg
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan 

import PhotoShake.Doneshooting

doneshootingSection :: Chan Msg.Message -> Doneshooting -> UI Element
doneshootingSection msgs x = do
    (_, picker) <- mkFolderPicker "doneshootingPicker" "Vælg config folder" $ \folder -> when (folder /= "") $ do
        liftIO $ Chan.writeChan msgs $ Msg.setDoneshooting $ yesDoneshooting folder

    doneshooting (mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Doneshooting mappe ikke valgt" # set (attr "id") "doneshootingMissing" ]
                            , mkColumn ["is-12"] [ element picker ]
                            ]
                      ] ) (\ y -> do

                        mkSection [ mkColumns ["is-multiline"]
                                        [ mkColumn ["is-12"] [ mkLabel "Doneshooting mappe" # set (attr "id") "doneshootingOK" ]
                                        , mkColumn ["is-12"] [ element picker ]
                                        , mkColumn ["is-12"] [ UI.p # set UI.text y # set (attr "id") "doneshootingPath" ]
                                        ]
                                  ] 
                  ) x
