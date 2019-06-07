{-# LANGUAGE OverloadedStrings #-}
module Photographer
    ( photographerSection
    ) where


import Prelude hiding (writeFile)

import PhotoShake.Photographer

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import Utils.ListZipper 

{- ups -}
import Shooting

import PhotoShake.ShakeConfig


photographerSection :: ShakeConfig -> UI Element
photographerSection config = do

        x <- liftIO $ getPhotographers config

        case x of
            NoPhotographers -> do
                    (_, importer) <- mkFilePicker "photographerPicker" "Vælg config fil" $ \file -> do
                            liftIO $ importPhotographers config file
                
                    mkSection [ mkLabel "Fotograf ikke valgt - importer fil"
                              , element importer
                              ]

            Photographers y -> do
                    let group = RadioGroup 
                            { action = \x _ -> do
                                    liftIO $ setPhotographers config $ Photographers x
                            , view' = \x -> UI.string (name (focus x))
                            , title' = "photographers"
                            , items = y 
                            }

                    select <- mkRadioGroup group

                    mkSection [ mkLabel "Fotograf"
                              , element select
                              ]
