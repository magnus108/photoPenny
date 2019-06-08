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

import Utils.Comonad
import Utils.ListZipper
import State (State, States(..), setStates)



photographerSection :: FilePath -> ListZipper State -> ShakeConfig -> UI Element
photographerSection root states config = do

        x <- liftIO $ getPhotographers config

        case x of
            NoPhotographers -> do
                    (_, importer) <- mkFilePicker "photographerPicker" "Vælg config fil" $ \file -> do
                            liftIO $ importPhotographers config file

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Fotograf ikke valgt - importer fil" ]
                                    , mkColumn ["is-12"] [ element importer ]
                                    ]
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

                    (buttonForward, forwardView) <- mkButton "next" "Ok"
                    on UI.click buttonForward $ \_ -> liftIO $ setStates root (States (forward states))

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Fotograf" ]
                                    , mkColumn ["is-12"] [ element select ]
                                    , mkColumn ["is-12"] [ element forwardView ]
                                    ]
                              ] 
