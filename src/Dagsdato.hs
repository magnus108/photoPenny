{-# LANGUAGE OverloadedStrings #-}
module Dagsdato
    (
    ) where

import Control.Concurrent.MVar

import Elements
import PhotoShake.Dagsdato

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Utils.ListZipper
import Utils.Actions
import Utils.FP
import PhotoShake.State (State, States(..), setStates)

import PhotoShake.ShakeConfig


