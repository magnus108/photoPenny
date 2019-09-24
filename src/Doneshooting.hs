{-# LANGUAGE OverloadedStrings #-}
module Doneshooting
    ( doneshootingSection
    ) where

import Control.Monad 

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements
import Menu

import qualified Message as Msg
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan 

import qualified PhotoShake.Doneshooting as Doneshooting
import qualified Utils.ListZipper as ListZipper
import qualified PhotoShake.State as State

doneshootingSection :: Element -> Chan Msg.Message -> ListZipper.ListZipper State.State -> Doneshooting.Doneshooting -> UI ()
doneshootingSection body msgs states doneshooting = do
    (_, picker) <- mkFolderPicker "doneshootingPicker" "Vælg config folder" $ \folder -> when (folder /= "") $ do
        liftIO $ Chan.writeChan msgs $ Msg.setDoneshooting $ Doneshooting.yesDoneshooting folder

    view <- Doneshooting.doneshooting (mkSection [ mkColumns ["is-multiline"]
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
                  ) doneshooting

    menu <- mkMenu msgs states 

    element body # set children [menu, view]

    return () 
