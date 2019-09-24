{-# LANGUAGE OverloadedStrings #-}
module DagsdatoBackup
    ( dagsdatoBackupSection
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import qualified Message as Msg

import Control.Monad 

import Elements
import Menu

import qualified PhotoShake.Dagsdato as Dagsdato

import qualified Utils.ListZipper as ListZipper
import qualified PhotoShake.State as State

dagsdatoBackupSection :: Element -> Chan Msg.Message -> ListZipper.ListZipper State.State -> Dagsdato.Dagsdato -> UI ()
dagsdatoBackupSection body msgs states dagsdato = do
    (_, picker) <- mkFolderPicker "doneshootingPicker" "Vælg config folder" $ \folder -> when (folder /= "") $ do
        liftIO $ Chan.writeChan msgs $ Msg.setDagsdatoBackup $ Dagsdato.yesDagsdato folder

    view <- Dagsdato.dagsdato ( mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dagsdato backup mappe ikke valgt" # set (attr "id") "dagsdatoBackupMissing" ]
                            , mkColumn ["is-12"] [ element picker ]
                            ]
                      ] )
                (\y -> do
                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Dagsdato backup mappe" # set (attr "id") "dagsdatoBackupOK" ]
                                    , mkColumn ["is-12"] [ element picker ]
                                    , mkColumn ["is-12"] [ UI.p # set UI.text y # set (attr "id") "dagsdatoBackupPath" ]
                                    ]
                              ] 
                ) dagsdato

    menu <- mkMenu msgs states 

    element body # set children [menu, view]

    return () 
