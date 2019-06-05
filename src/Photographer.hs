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


photographerSection :: ShakeConfig -> UI (Bool, Element)
photographerSection config = do
        x <- liftIO $ getPhotographers config
        case x of
            NoPhotographers -> do
                    gg <- mkSection [ mkLabel "Fotograf ikke valgt - importer fil"
                                    , mkPhotographersImporter config
                                    ]
                    return (False, gg)

            Photographers y -> do
                        gg <- mkSection [ mkLabel "Fotograf"
                                        , mkSelectPhotographers config y
                                        ]
                        return (True, gg)



mkPhotographersImporter :: ShakeConfig -> UI Element
mkPhotographersImporter config = do
    (_, view) <- mkFilePicker "photographerPicker" "Vælg config fil" $ \file -> do
        liftIO $ importPhotographers config file
    return view


mkSelectPhotographers :: ShakeConfig -> ListZipper Photographer -> UI Element
mkSelectPhotographers config y = do 
    let group' = RadioGroup 
            { action = \x _ -> do
                    liftIO $ setPhotographers config $ Photographers x
            , view' = \x -> UI.string (name (focus x))
            , title' = "photographers"
            , items = y 
            }
    view <- mkRadioGroup group'
    return view
