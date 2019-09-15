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
    , _setDoneshooting
    , _setDagsdato
    , _setPhotographers
    , _dump
    , _doneshooting
    , _dagsdato
    , _photographers
    , _stateFile
    , _dumpFile
    , _photographerFile
    , _doneshootingFile
    , _dagsdatoFile
    , _shakeConfig
    , _root
    ) where

import Utils.Env
import Utils.Comonad


import PhotoShake.State

import Utils.FP
import qualified PhotoShake.Dump as D
import qualified PhotoShake.Doneshooting as DO
import qualified PhotoShake.Dagsdato as DA
import qualified PhotoShake.Photographer as Photographer

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

    , dagsdato :: DA.Dagsdato

    , doneshooting :: DO.Doneshooting
    , photographers :: Photographer.Photographers


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

_doneshootingFile :: App Model -> FilePath -- deleteme
_doneshootingFile = _doneshootingConfig . _shakeConfig


_photographerFile :: App Model -> FilePath -- deleteme
_photographerFile = _photographerConfig . _shakeConfig

_dagsdatoFile :: App Model -> FilePath -- deleteme
_dagsdatoFile = _dagsdatoConfig . _shakeConfig

_states:: App Model -> Maybe States -- deleteme
_states = states . extract . unApp


_setStates :: App Model -> Maybe States -> App Model -- deleteme
_setStates x (Just s) = App $ (unApp x) =>> (\x -> (extract x) { states = Just s } )
_setStates x Nothing = App $ (unApp x) =>> (\x -> (extract x) { states = Nothing } )


_setDump :: App Model -> D.Dump -> App Model -- deleteme
_setDump x y = App $ (unApp x) =>> (\x -> (extract x) { dump = y } )

_setDoneshooting :: App Model -> DO.Doneshooting -> App Model -- deleteme
_setDoneshooting x y = App $ (unApp x) =>> (\x -> (extract x) { doneshooting = y } )

_setDagsdato :: App Model -> DA.Dagsdato -> App Model -- deleteme
_setDagsdato x y = App $ (unApp x) =>> (\x -> (extract x) { dagsdato = y } )

_setPhotographers :: App Model -> Photographer.Photographers -> App Model -- deleteme
_setPhotographers x y = App $ (unApp x) =>> (\x -> (extract x) { photographers = y } )

_dump :: App Model -> D.Dump -- deleteme
_dump = dump . extract . unApp

_doneshooting :: App Model -> DO.Doneshooting -- deleteme
_doneshooting = doneshooting . extract . unApp

_dagsdato :: App Model -> DA.Dagsdato -- deleteme
_dagsdato = dagsdato . extract . unApp

_photographers :: App Model -> Photographer.Photographers -- deleteme
_photographers = photographers . extract . unApp


_shakeConfig :: App Model -> ShakeConfig -- deleteme
_shakeConfig = shakeConfig . extract . unApp
