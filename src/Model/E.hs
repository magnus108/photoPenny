{-# LANGUAGE OverloadedStrings #-}
module Model.E
    ( E
    , production
    , test
    , unApp
    , App
    , app
    , Model
    , model

    , _configs 
    , _states
    , _setStates
    , _setDump
    , _dump
    , _stateFile
    , _dumpFile
    , _shakeConfig
    , _root
    ) where

import Utils.Env
import Utils.Comonad


import State

import Utils.FP
import qualified PhotoShake.Dump as D
import PhotoShake.ShakeConfig 

data E = Production | Test

production :: E
production = Production

test :: E
test = Test

newtype App a = App { unApp :: Env E a }



-- lens me
data Model = Model 
    { states :: Maybe States 

    , dump :: D.Dump

    , dir1 :: FilePath -- deleteme
    , root :: FP -- deleteme

    , shakeConfig :: ShakeConfig --question me
    }







app = App

model = Model








_root :: App Model -> FP -- deleteme
_root = root . extract . unApp

_configs :: App Model -> FilePath -- deleteme
_configs = dir1 . extract . unApp

_stateFile :: App Model -> FilePath -- deleteme
_stateFile  = _stateConfig . _shakeConfig

_dumpFile :: App Model -> FilePath -- deleteme
_dumpFile  = _dumpConfig . _shakeConfig

_states:: App Model -> Maybe States -- deleteme
_states = states . extract . unApp


_setStates :: App Model -> Maybe States -> App Model -- deleteme
_setStates x (Just s) = App $ (unApp x) =>> (\x -> (extract x) { states = Just s } )
_setStates x Nothing = App $ (unApp x) =>> (\x -> (extract x) { states = Nothing } )


_setDump :: App Model -> D.Dump -> App Model -- deleteme
_setDump x y = App $ (unApp x) =>> (\x -> (extract x) { dump = y } )

_dump :: App Model -> D.Dump -- deleteme
_dump = dump . extract . unApp


_shakeConfig :: App Model -> ShakeConfig -- deleteme
_shakeConfig = shakeConfig . extract . unApp
