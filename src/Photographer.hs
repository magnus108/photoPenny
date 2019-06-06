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

            UnApprovedPhotographers y -> do
                        gg <- mkSection [ mkLabel "Fotograf"
                                        , mkApprovedPhotographers config y $ \z _ ->
                                                liftIO $ setPhotographers config $ ApprovedPhotographers z
                                        ]
                        return (False, gg)

            ApprovedPhotographers y -> do
                        gg <- mkSection [ mkLabel "Fotograf"
                                        , mkSelectPhotographers config y
                                        ]
                        return (True, gg)



mkPhotographersImporter :: ShakeConfig -> UI Element
mkPhotographersImporter config = do
    (_, view) <- mkFilePicker "photographerPicker" "Vælg config fil" $ \file -> do
        liftIO $ importPhotographers config file
    return view

mkApprovedPhotographers :: ShakeConfig -> ListZipper Photographer -> (ListZipper Photographer-> () -> UI ()) -> UI Element
mkApprovedPhotographers config y cb = do
    (button, buttonView) <- mkButton "approvePhotographers" "ok"

    on UI.click button (cb y)

    view <- UI.div #+
                [ mkSelectPhotographers config y
                , element buttonView
                ]
    return view



mkSelectPhotographers :: ShakeConfig -> ListZipper Photographer -> UI Element
mkSelectPhotographers config y = do 
    let group' = RadioGroup 
            { action = \x _ -> do
                    liftIO $ setPhotographers config $ ApprovedPhotographers x
            , view' = \x -> UI.string (name (focus x))
            , title' = "photographers"
            , items = y 
            }
    view <- mkRadioGroup group'
    return view
