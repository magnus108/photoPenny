{-# LANGUAGE OverloadedStrings #-}
module Model.E
    ( E
    , production
    , test
    , unApp
    , App
    , app
    , Model (..)
    , model

    , _subscriptions
    , _configs 
    , _states
    , _cancel
    , _location
    , _setStates
    , _setLocation
    , _setDump
    , _setCancel
    , _setDoneshooting
    , _setDagsdato
    , _setPhotographers
    , _setGrades
    , _dump
    , _doneshooting
    , _shootings
    , _sessions
    , _dagsdato
    , _photographers
    , _grades
    , _setShootings
    , _setSessions
    , _locationFile
    , _shootingFile
    , _sessionFile
    , _stateFile

    , _gradesFile

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
import qualified PhotoShake.Session as Session
import qualified PhotoShake.Shooting as Shooting
import qualified PhotoShake.Location as Location
import qualified PhotoShake.Grade as Grade

import PhotoShake.ShakeConfig 


import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import System.FSNotify hiding (defaultConfig)
import qualified Message as Msg


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
    , shootings :: Shooting.Shootings
    , sessions :: Session.Sessions

    , location :: Location.Location

    , grades :: Grade.Grades


    , dir1 :: FilePath -- deleteme
    , root :: FP -- deleteme

    , shakeConfig :: ShakeConfig --question me
    , subscriptions :: WatchManager -> Chan Msg.Message -> MVar (App Model) -> IO StopListening -- ??? 
    , cancel :: StopListening
    }







app = App

model = Model






_root :: App Model -> FP -- deleteme
_root = root . extract . unApp


_subscriptions :: App Model -> (WatchManager -> Chan Msg.Message -> MVar (App Model) -> IO StopListening) -- ???
_subscriptions = subscriptions . extract . unApp


_configs :: App Model -> FilePath -- deleteme
_configs = dir1 . extract . unApp

_stateFile :: App Model -> FilePath -- deleteme
_stateFile  = _stateConfig . _shakeConfig


_gradesFile :: App Model -> FilePath -- deleteme
_gradesFile  = _gradeConfig . _shakeConfig

_dumpFile :: App Model -> FilePath -- deleteme
_dumpFile  = _dumpConfig . _shakeConfig

_doneshootingFile :: App Model -> FilePath -- deleteme
_doneshootingFile = _doneshootingConfig . _shakeConfig


_photographerFile :: App Model -> FilePath -- deleteme
_photographerFile = _photographerConfig . _shakeConfig

_dagsdatoFile :: App Model -> FilePath -- deleteme
_dagsdatoFile = _dagsdatoConfig . _shakeConfig

_sessionFile :: App Model -> FilePath -- deleteme
_sessionFile = _sessionConfig . _shakeConfig

_shootingFile :: App Model -> FilePath -- deleteme
_shootingFile = _shootingsConfig . _shakeConfig


_locationFile :: App Model -> FilePath -- deleteme
_locationFile = _locationConfig . _shakeConfig

_states:: App Model -> Maybe States -- deleteme
_states = states . extract . unApp

_location :: App Model -> Location.Location -- deleteme
_location = location . extract . unApp

_cancel :: App Model -> StopListening -- deleteme
_cancel = cancel . extract . unApp

_setStates :: App Model -> Maybe States -> App Model -- deleteme
_setStates x (Just s) = App $ (unApp x) =>> (\x -> (extract x) { states = Just s } )
_setStates x Nothing = App $ (unApp x) =>> (\x -> (extract x) { states = Nothing } )

_setCancel :: App Model -> StopListening -> App Model -- deleteme
_setCancel x y = App $ (unApp x) =>> (\x -> (extract x) { cancel = y } )

_setDump :: App Model -> D.Dump -> App Model -- deleteme
_setDump x y = App $ (unApp x) =>> (\x -> (extract x) { dump = y } )

_setLocation :: App Model -> Location.Location -> App Model -- deleteme
_setLocation x y = App $ (unApp x) =>> (\x -> (extract x) { location = y } )

_setDoneshooting :: App Model -> DO.Doneshooting -> App Model -- deleteme
_setDoneshooting x y = App $ (unApp x) =>> (\x -> (extract x) { doneshooting = y } )

_setDagsdato :: App Model -> DA.Dagsdato -> App Model -- deleteme
_setDagsdato x y = App $ (unApp x) =>> (\x -> (extract x) { dagsdato = y } )

_setPhotographers :: App Model -> Photographer.Photographers -> App Model -- deleteme
_setPhotographers x y = App $ (unApp x) =>> (\x -> (extract x) { photographers = y } )

_setSessions :: App Model -> Session.Sessions -> App Model -- deleteme
_setSessions x y = App $ (unApp x) =>> (\x -> (extract x) { sessions = y } )

_setShootings :: App Model -> Shooting.Shootings -> App Model -- deleteme
_setShootings x y = App $ (unApp x) =>> (\x -> (extract x) { shootings = y } )


_setGrades :: App Model -> Grade.Grades -> App Model -- deleteme
_setGrades x y = App $ (unApp x) =>> (\x -> (extract x) { grades = y } )


_dump :: App Model -> D.Dump -- deleteme
_dump = dump . extract . unApp

_doneshooting :: App Model -> DO.Doneshooting -- deleteme
_doneshooting = doneshooting . extract . unApp

_dagsdato :: App Model -> DA.Dagsdato -- deleteme
_dagsdato = dagsdato . extract . unApp


_sessions :: App Model -> Session.Sessions -- deleteme
_sessions = sessions . extract . unApp

_shootings :: App Model -> Shooting.Shootings -- deleteme
_shootings = shootings . extract . unApp


_photographers :: App Model -> Photographer.Photographers -- deleteme
_photographers = photographers . extract . unApp

_grades :: App Model -> Grade.Grades -- deleteme
_grades  = grades . extract . unApp


_shakeConfig :: App Model -> ShakeConfig -- deleteme
_shakeConfig = shakeConfig . extract . unApp
