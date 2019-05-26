{-# LANGUAGE OverloadedStrings #-}
module Shooting
    ( mkRadioGroup
    , shootingSection
    ) where

import Prelude hiding (writeFile)
import Data.ByteString.Lazy hiding (take)

import PhotoShake.Shooting

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import Control.Arrow
import Utils.ListZipper 
import Utils.Comonad

import PhotoShake.ShakeConfig


shootingSection :: FilePath -> FilePath -> UI Element
shootingSection config shootingConfig = mkSection
                                [ mkLabel "Shooting Type"
                                , mkRadioShootings config shootingConfig
                                ]


mkRadioShootings :: FilePath -> FilePath -> UI Element
mkRadioShootings _ conf = do 
    shootings <- liftIO $ getShootings conf
    view <- mkRadioGroup "foobar" conf (unShootings shootings)
    return view


mkRadioGroup :: String -> FilePath -> ListZipper Shooting -> UI Element
mkRadioGroup name config xs = do
    let labels x = UI.string (show (focus x))
    let inputs x = do
            y <- UI.input # set UI.type_ "radio" # set UI.name name
            on UI.checkedChange y $ \_ -> do
                    liftIO $ writeFile config $ encode (Shootings x)
                    return ()
            return y

    let displays = extend (labels &&& inputs) xs
    let selected x = x # set (UI.attr "checked") "true" 
    let withSelected = mapFocus (second selected) displays

    let radio (x,y) = UI.label #. "radio" #+ [y, x]

    let widgets = fmap radio withSelected
    let l = lefts widgets
    let x = focus widgets
    let r = rights widgets

    view <- UI.div #. "control" #+ (l ++ [x] ++ r)


    return view
