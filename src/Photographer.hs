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

import State (State, States(..), setStates)



photographerSection :: FilePath -> FilePath -> ListZipper State -> ShakeConfig -> UI Element
photographerSection root stateFile states config = do

        x <- liftIO $ getPhotographers config

        (_, importer) <- mkFilePicker "photographerPicker" "Vælg import fil" $ \file -> do
                liftIO $ importPhotographers config file

        case x of
            NoPhotographers -> do

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Fotograf ikke valgt - importer fil" ]
                                    , mkColumn ["is-12"] [ element importer ]
                                    ]
                              ] 

            Photographers y -> do
                    let group = RadioGroup 
                            { action = \xx _ -> do
                                    liftIO $ setPhotographers config $ Photographers xx
                            , view' = \xx -> UI.string (name (focus xx))
                            , title' = "photographers"
                            , items = y 
                            }

                    select <- mkRadioGroup group

                    (buttonForward, forwardView) <- mkButton "next" "Ok"
                    on UI.click buttonForward $ \_ -> liftIO $ setStates root stateFile (States (forward states))

                    mkSection [ mkColumns ["is-multiline"]
                                    [ mkColumn ["is-12"] [ mkLabel "Fotograf" # set (attr "id") "photographerOK" ]
                                    , mkColumn ["is-12"] [ element select ]
                                    , mkColumn ["is-12"] [ element importer ]
                                    , mkColumn ["is-12"] [ element forwardView ]
                                    ]
                              ] 
