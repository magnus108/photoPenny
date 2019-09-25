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

    , _actionGrades
    , _actionLocation
    , _actionDumpFiles
    , _actionGetBuild
    , _subscriptions
    , _configs 
    , _states
    , _dumpFiles
    , _buildFile
    , _photographee
    , _photographees
    , _cancel
    , _cancelDumpFiles
    , _cancelControl
    , _cancelLocation
    , _location
    , _build
    , _setId
    , _setPhotographee
    , _setPhotographees
    , _setStates
    , _setBuild
    , _setControl
    , _setLocation
    , _setDump
    , _setDumpFiles
    , _setCancel
    , _setCancelDumpFiles
    , _setCancelControl
    , _setCancelLocation
    , _setDoneshooting
    , _setDagsdato
    , _setDagsdatoBackup
    , _setPhotographers
    , _setGrades
    , _dump
    , _control
    , _doneshooting
    , _shootings
    , _sessions
    , _dagsdato
    , _dagsdatoBackup
    , _photographers
    , _grades
    , _id
    , _setShootings
    , _setSessions
    , _locationFile
    , _shootingFile
    , _sessionFile
    , _stateFile
    , _idFile

    , _gradesFile

    , _dumpFile
    , _photographerFile
    , _doneshootingFile
    , _dagsdatoFile
    , _dagsdatoBackupFile
    , _shakeConfig
    , _root
    ) where

import Utils.Env
import Utils.Comonad


import Prelude hiding (id)

import PhotoShake.State

import Utils.FP
import qualified PhotoShake.Dump as D
import qualified PhotoShake.Doneshooting as DO
import qualified PhotoShake.Dagsdato as DA
import qualified PhotoShake.Photographer as Photographer
import qualified PhotoShake.Photographee2 as Photographee
import qualified PhotoShake.Session as Session
import qualified PhotoShake.Shooting as Shooting
import qualified PhotoShake.Location as Location
import qualified PhotoShake.Grade as Grade
import qualified PhotoShake.Id as Id
import qualified PhotoShake.Control as Control
import qualified PhotoShake.Build as Build

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
    , dumpFiles :: DumpFiles --bads
    , photographee :: Maybe Photographee.Photographee --bads
    , photographees :: [Photographee.Photographee] --bads

    , dagsdato :: DA.Dagsdato
    , dagsdatoBackup :: DA.Dagsdato
    , build :: Build.Build

    , doneshooting :: DO.Doneshooting
    , photographers :: Photographer.Photographers
    , shootings :: Shooting.Shootings
    , sessions :: Session.Sessions

    , location :: Location.Location
    , actionGetBuild :: IO ()
    , actionDumpFiles :: IO ()

    , actionGrades :: IO ()
    , actionLocation :: IO ()

    , grades :: Grade.Grades
    , id :: Id.Id


    , dir1 :: FilePath -- deleteme
    , root :: FP -- deleteme
    , control :: Control.Result -- deleteme

    , shakeConfig :: ShakeConfig --question me
    , subscriptions :: WatchManager -> Chan Msg.Message -> App Model -> IO (StopListening, StopListening, StopListening, StopListening) -- ??? 
    , cancel :: StopListening
    , cancelDumpFiles :: StopListening
    , cancelControl :: StopListening
    , cancelLocation :: StopListening
    }







app = App

model = Model






_root :: App Model -> FP -- deleteme
_root = root . extract . unApp


_subscriptions :: App Model -> (WatchManager -> Chan Msg.Message -> App Model -> IO (StopListening, StopListening, StopListening, StopListening)) -- ???
_subscriptions = subscriptions . extract . unApp


_configs :: App Model -> FilePath -- deleteme
_configs = dir1 . extract . unApp

_stateFile :: App Model -> FilePath -- deleteme
_stateFile  = _stateConfig . _shakeConfig


_buildFile :: App Model -> FilePath -- deleteme
_buildFile  = _buildConfig . _shakeConfig


_idFile :: App Model -> FilePath -- deleteme
_idFile  = _idConfig . _shakeConfig


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

_dagsdatoBackupFile :: App Model -> FilePath -- deleteme
_dagsdatoBackupFile = _dagsdatoBackupConfig . _shakeConfig

_sessionFile :: App Model -> FilePath -- deleteme
_sessionFile = _sessionConfig . _shakeConfig

_shootingFile :: App Model -> FilePath -- deleteme
_shootingFile = _shootingsConfig . _shakeConfig


_locationFile :: App Model -> FilePath -- deleteme
_locationFile = _locationConfig . _shakeConfig

_states:: App Model -> Maybe States -- deleteme
_states = states . extract . unApp


_dumpFiles :: App Model -> DumpFiles -- deleteme
_dumpFiles = dumpFiles . extract . unApp

_location :: App Model -> Location.Location -- deleteme
_location = location . extract . unApp

_build :: App Model -> Build.Build -- deleteme
_build = build . extract . unApp

_cancel :: App Model -> StopListening -- deleteme
_cancel = cancel . extract . unApp

_cancelDumpFiles :: App Model -> StopListening -- deleteme
_cancelDumpFiles = cancelDumpFiles . extract . unApp

_cancelControl :: App Model -> StopListening -- deleteme
_cancelControl = cancelControl . extract . unApp

_cancelLocation :: App Model -> StopListening -- deleteme
_cancelLocation = cancelLocation . extract . unApp


_setStates :: App Model -> Maybe States -> App Model -- deleteme
_setStates x (Just s) = App $ (unApp x) =>> (\x -> (extract x) { states = Just s } )
_setStates x Nothing = App $ (unApp x) =>> (\x -> (extract x) { states = Nothing } )

_setCancel :: App Model -> StopListening -> App Model -- deleteme
_setCancel x y = App $ (unApp x) =>> (\x -> (extract x) { cancel = y } )


_setBuild :: App Model -> Build.Build -> App Model -- deleteme
_setBuild x y = App $ (unApp x) =>> (\x -> (extract x) { build = y } )


_setCancelDumpFiles :: App Model -> StopListening -> App Model -- deleteme
_setCancelDumpFiles x y = App $ (unApp x) =>> (\x -> (extract x) { cancelDumpFiles = y } )

_setCancelControl :: App Model -> StopListening -> App Model -- deleteme
_setCancelControl x y = App $ (unApp x) =>> (\x -> (extract x) { cancelControl = y } )

_setCancelLocation :: App Model -> StopListening -> App Model -- deleteme
_setCancelLocation x y = App $ (unApp x) =>> (\x -> (extract x) { cancelLocation = y } )

_setDumpFiles:: App Model -> DumpFiles -> App Model -- deleteme
_setDumpFiles x y = App $ (unApp x) =>> (\x -> (extract x) { dumpFiles = y } )


_setControl :: App Model -> Control.Result -> App Model -- deleteme
_setControl x y = App $ (unApp x) =>> (\x -> (extract x) { control = y } )

_setDump :: App Model -> D.Dump -> App Model -- deleteme
_setDump x y = App $ (unApp x) =>> (\x -> (extract x) { dump = y } )

_setLocation :: App Model -> Location.Location -> App Model -- deleteme
_setLocation x y = App $ (unApp x) =>> (\x -> (extract x) { location = y } )

_setId :: App Model -> Id.Id -> App Model -- deleteme
_setId x y = App $ (unApp x) =>> (\x -> (extract x) { id = y } )

_setDoneshooting :: App Model -> DO.Doneshooting -> App Model -- deleteme
_setDoneshooting x y = App $ (unApp x) =>> (\x -> (extract x) { doneshooting = y } )

_setPhotographee :: App Model -> Maybe Photographee.Photographee -> App Model -- deleteme
_setPhotographee x y = App $ (unApp x) =>> (\x -> (extract x) { photographee = y } )

_setPhotographees :: App Model -> [Photographee.Photographee] -> App Model -- deleteme
_setPhotographees x y = App $ (unApp x) =>> (\x -> (extract x) { photographees = y } )

_setDagsdato :: App Model -> DA.Dagsdato -> App Model -- deleteme
_setDagsdato x y = App $ (unApp x) =>> (\x -> (extract x) { dagsdato = y } )

_setDagsdatoBackup :: App Model -> DA.Dagsdato -> App Model -- deleteme
_setDagsdatoBackup x y = App $ (unApp x) =>> (\x -> (extract x) { dagsdatoBackup = y } )

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

_control :: App Model -> Control.Result -- deleteme
_control = control . extract . unApp

_doneshooting :: App Model -> DO.Doneshooting -- deleteme
_doneshooting = doneshooting . extract . unApp

_photographee :: App Model -> Maybe Photographee.Photographee -- deleteme
_photographee = photographee . extract . unApp

_photographees :: App Model -> [Photographee.Photographee] -- deleteme
_photographees = photographees . extract . unApp

_dagsdato :: App Model -> DA.Dagsdato -- deleteme
_dagsdato = dagsdato . extract . unApp

_dagsdatoBackup :: App Model -> DA.Dagsdato -- deleteme
_dagsdatoBackup = dagsdatoBackup . extract . unApp

_sessions :: App Model -> Session.Sessions -- deleteme
_sessions = sessions . extract . unApp

_shootings :: App Model -> Shooting.Shootings -- deleteme
_shootings = shootings . extract . unApp


_photographers :: App Model -> Photographer.Photographers -- deleteme
_photographers = photographers . extract . unApp

_grades :: App Model -> Grade.Grades -- deleteme
_grades  = grades . extract . unApp


_id :: App Model -> Id.Id -- deleteme
_id = id . extract . unApp

_actionLocation :: App Model -> IO ()-- deleteme
_actionLocation = actionLocation . extract . unApp

_actionGrades :: App Model -> IO ()-- deleteme
_actionGrades = actionGrades . extract . unApp

_actionDumpFiles :: App Model -> IO ()-- deleteme
_actionDumpFiles = actionDumpFiles . extract . unApp

_actionGetBuild :: App Model -> IO ()-- deleteme
_actionGetBuild = actionGetBuild . extract . unApp

_shakeConfig :: App Model -> ShakeConfig -- deleteme
_shakeConfig = shakeConfig . extract . unApp
