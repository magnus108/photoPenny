{-# LANGUAGE OverloadedStrings #-}
module Shooting
    ( mkRadioGroup
    , shootingSection
    -- ups
    , RadioGroup(..)
    ) where


import PhotoShake.Shooting

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import Utils.ListZipper 
import Utils.Comonad

import PhotoShake.ShakeConfig


shootingSection :: ShakeConfig -> UI (Bool, Element)
shootingSection config = do
        x <- liftIO $ getShootings config
        case x of
            NoShootings-> do
                    gg <- mkSection [ mkLabel "Shooting ikke valgt"
                                    , mkShootingsImporter config
                                    ]
                    return (False, gg)

            Shootings y -> do
                        gg <- mkSection [ mkLabel "Shooting type"
                                        , mkRadioShootings config y
                                        ]
                        return (True, gg)


mkRadioShootings :: ShakeConfig -> ListZipper Shooting -> UI Element
mkRadioShootings config y = do 
    let group' = RadioGroup 
            { action = \x _ -> do
                    liftIO $ setShooting config $ Shootings x
            , view' = \x -> UI.string (show (focus x))
            , title' = "shootings"
            , items = y
            }
    view <- mkRadioGroup group'
    return view

mkShootingsImporter :: ShakeConfig -> UI Element
mkShootingsImporter config = do
    (_, view) <- mkFilePicker "Vælg config fil" $ \file -> do
        liftIO $ importShootings config file
    return view

mkRadioGroup :: Eq a => RadioGroup a -> UI Element
mkRadioGroup x = do
    let widgets = extend 
            (\zipper -> do
                    input' <- UI.input # set UI.type_ "radio" # set UI.name (title' x) 
                    input <- if (zipper == (items x)) then
                            set (UI.attr "checked") "" (element input')
                        else
                            return input'
                        
                    on UI.checkedChange input ((action x) zipper)
                    label <- (view' x) zipper  
                    view <- UI.label #. "radio" #+ [element input, element label]
                    return view
            ) (items x)

    view <- UI.div #. "control" #+ (toList widgets)
    return view


data RadioGroup a = RadioGroup
    { action :: ListZipper a -> Bool -> UI ()
    , view' :: ListZipper a -> UI Element
    , title' :: String
    , items :: ListZipper a
    }
