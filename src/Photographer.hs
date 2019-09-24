{-# LANGUAGE OverloadedStrings #-}
module Photographer
    ( photographerSection
    ) where

import Control.Monad 

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements
import Menu

import qualified Message as Msg
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan 

import qualified PhotoShake.Photographer as Photographer

import Utils.FP
import Utils.Actions


import qualified PhotoShake.State as State
import qualified Utils.ListZipper as ListZipper

import Shooting -- deleteme

photographerSection :: Element -> Chan Msg.Message -> ListZipper.ListZipper State.State -> Photographer.Photographers -> UI ()
photographerSection body msgs states photographers   = do

    (_, picker) <- mkFilePicker "photographerPicker" "Vælg import fil" $ \file -> when (file /= "") $ do
        photographers <- liftIO $ interpret $ Photographer.getPhotographers $ fp $ start $ file
        liftIO $ Chan.writeChan msgs $ Msg.setPhotographers photographers

    view <- Photographer.photographers ( mkSection [ mkColumns ["is-multiline"]
                                [ mkColumn ["is-12"] [ mkLabel "Fotograf ikke valgt - importer fil" # set (attr "id") "photographersMissing" ]
                                , mkColumn ["is-12"] [ element picker ]
                                ]
                          ]) (\y -> do
                            let group = RadioGroup 
                                    { action = \xxx _ -> do
                                            liftIO $ Chan.writeChan msgs $ Msg.setPhotographers $ Photographer.yesPhotographers xxx
                                    , view' = \xxx -> UI.string (Photographer._name (ListZipper.focus xxx))
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
                        ) photographers

    menu <- mkMenu msgs states 

    element body # set children [menu, view]

    return () 
