{-# LANGUAGE OverloadedStrings #-}
module Shooting
    ( mkRadioGroup
    , shootingSection
    ) where

import Prelude hiding (writeFile)
import Data.ByteString.Lazy hiding (take, putStrLn)

import PhotoShake.Shooting

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import Utils.ListZipper 
import Utils.Comonad

import PhotoShake.ShakeConfig


shootingSection :: FilePath -> FilePath -> UI Element
shootingSection root config = mkSection
                                [ mkLabel "Shooting Type"
                                , mkRadioShootings root config
                                ]


mkRadioShootings :: FilePath -> FilePath -> UI Element
mkRadioShootings _ config = do 
    shootings <- liftIO $ getShootings config
    let group' = RadioGroup 
            { action = \x _ -> do
                    liftIO $ writeFile config $ encode (Shootings x)
                    return ()
            , view' = \x -> UI.string (show (focus x))
            , title' = "foobar"
            , items = unShootings shootings
            }
    view <- mkRadioGroup group'
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
