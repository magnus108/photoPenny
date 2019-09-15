{-# LANGUAGE OverloadedStrings #-}
module Photographer
    ( photographerSection
    ) where

import Control.Monad 

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import qualified Message as Msg
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan 

import PhotoShake.Photographer

import Utils.FP
import Utils.Actions
import Utils.ListZipper



import Shooting -- deleteme

photographerSection :: Chan Msg.Message -> Photographers -> UI Element
photographerSection msgs x = do

    (_, picker) <- mkFilePicker "photographerPicker" "Vælg import fil" $ \file -> when (file /= "") $ do
        photographers <- liftIO $ interpret $ getPhotographers $ fp $ start $ file
        liftIO $ Chan.writeChan msgs $ Msg.setPhotographers photographers

    photographers ( mkSection [ mkColumns ["is-multiline"]
                                [ mkColumn ["is-12"] [ mkLabel "Fotograf ikke valgt - importer fil" # set (attr "id") "photographersMissing" ]
                                , mkColumn ["is-12"] [ element picker ]
                                ]
                          ]) (\y -> do
                            let group = RadioGroup 
                                    { action = \xxx _ -> do
                                            liftIO $ Chan.writeChan msgs $ Msg.setPhotographers $ yesPhotographers xxx
                                    , view' = \xxx -> UI.string (_name (focus xxx))
                                    , title' = "photographers"
                                    , items = y 
                                    }

                            select <- mkRadioGroup group

                            mkSection [ mkColumns ["is-multiline"]
                                            [ mkColumn ["is-12"] [ mkLabel "Fotograf" # set (attr "id") "photographerOK" ]
                                            , mkColumn ["is-12"] [ element select ]
                                            , mkColumn ["is-12"] [ element picker ]
                                            ]
                                      ] 
                        ) x
