{-# LANGUAGE OverloadedStrings #-}
module Photographer
    ( photographerSection
    ) where

import Prelude hiding (writeFile)
import Data.ByteString.Lazy hiding (take, putStrLn)

import PhotoShake.Photographer

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import Utils.ListZipper 
{- ups -}
import Shooting
import PhotoShake.Shooting


import PhotoShake.ShakeConfig


photographerSection :: FilePath -> FilePath -> UI Element
photographerSection root config = mkSection
                                [ mkLabel "Fotograf"
                                , mkSelectPhotographers root config
                                ]


mkSelectPhotographers :: FilePath -> FilePath -> UI Element
mkSelectPhotographers _ config = do 
    photographers <- liftIO $ getPhotographers config
    let group' = RadioGroup 
            { action = \x _ -> do
                    liftIO $ writeFile config $ encode (Photographers x)
                    return ()
            , view' = \x -> UI.string (name (focus x))
            , title' = "photographers"
            , items = unPhotographers photographers
            }
    view <- mkRadioGroup group'
    return view
