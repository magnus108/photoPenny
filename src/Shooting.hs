{-# LANGUAGE OverloadedStrings #-}
module Shooting
    ( mkRadioGroup
    , shootingSection
    , shootingParse
    ) where

import PhotoShake.Shooting


import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements


import Control.Arrow
import Utils.ListZipper 


shootingParse :: Shooting -> ListZipper Shooting
shootingParse _ = ListZipper [] Normal [Omfoto]


shootingSection :: FilePath -> ListZipper Shooting -> UI Element
shootingSection _ zipper = mkSection
                                [ mkLabel "Shooting Type"
                                , mkRadioGroup "foobar" zipper
                                ]


mkRadioGroup :: String -> ListZipper Shooting -> UI Element
mkRadioGroup name xs = do
    let labels x = UI.string (show x)
    let inputs _ = UI.input # set UI.type_ "radio" # set UI.name name
    let displays = fmap (labels &&& inputs) xs
    let selected x = x # set (UI.attr "checked") "true" 
    let withSelected = mapFocus (second selected) displays
    let radio (x,y) = UI.label #. "radio" #+ [y, x]
    let widgets = fmap radio withSelected
    let l = lefts widgets
    let x = selected $ focus widgets
    let r = rights widgets

    view <- UI.div #. "control" #+ (l ++ [x] ++ r)
    return view
