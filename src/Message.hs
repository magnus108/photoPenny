module Message
    ( Message(..) -- bad
    , block
    , getStates
    , setStates
    , getDump
    , setDump
    , getDoneshooting
    , setDoneshooting
    , getDagsdato
    , setDagsdato
    , getPhotographers
    , setPhotographers
    , getShootings
    , setShootings
    , getSessions
    , setSessions
    , getLocation
    , setLocation
    , getGrades
    , setGrades
    , getId
    , setId
    , getDumpFiles
    , getPhotographee
    ) where


import PhotoShake.State (States)

import Control.Concurrent.MVar

import qualified PhotoShake.Dump as Dump
import qualified PhotoShake.Doneshooting as Doneshooting 
import qualified PhotoShake.Dagsdato as Dagsdato
import qualified PhotoShake.Photographer as Photographer
import qualified PhotoShake.Shooting as Shooting
import qualified PhotoShake.Session as Session
import qualified PhotoShake.Location as Location
import qualified PhotoShake.Grade as Grade
import qualified PhotoShake.Id as Id

data Message 
    = Block (MVar ())

    | GetStates
    | SetStates States

    | GetId
    | SetId Id.Id

    | GetDump
    | SetDump Dump.Dump

    | GetDoneshooting
    | SetDoneshooting Doneshooting.Doneshooting

    | GetDagsdato
    | SetDagsdato Dagsdato.Dagsdato

    | GetPhotographers
    | SetPhotographers Photographer.Photographers

    | GetSessions
    | SetSessions Session.Sessions

    | GetShootings
    | SetShootings Shooting.Shootings

    | GetLocation
    | SetLocation Location.Location

    | GetGrades
    | SetGrades Grade.Grades

    | GetDumpFiles
    | GetPhotographee



block :: MVar () -> Message
block = Block

getStates :: Message
getStates = GetStates

setStates :: States -> Message
setStates = SetStates


getDump :: Message
getDump = GetDump

setDump :: Dump.Dump -> Message
setDump = SetDump


getDoneshooting :: Message
getDoneshooting = GetDoneshooting

setDoneshooting :: Doneshooting.Doneshooting -> Message
setDoneshooting = SetDoneshooting


getDagsdato :: Message
getDagsdato = GetDagsdato

setDagsdato :: Dagsdato.Dagsdato -> Message
setDagsdato = SetDagsdato


getPhotographers :: Message
getPhotographers = GetPhotographers

setPhotographers :: Photographer.Photographers -> Message
setPhotographers = SetPhotographers


getSessions :: Message
getSessions = GetSessions

setSessions :: Session.Sessions -> Message
setSessions = SetSessions


getShootings :: Message
getShootings = GetShootings

setShootings :: Shooting.Shootings -> Message
setShootings = SetShootings


getLocation :: Message
getLocation = GetLocation

setLocation :: Location.Location -> Message
setLocation = SetLocation

getGrades :: Message
getGrades = GetGrades

setGrades :: Grade.Grades -> Message
setGrades = SetGrades


getId :: Message
getId = GetId

setId :: Id.Id -> Message
setId = SetId


getDumpFiles :: Message
getDumpFiles = GetDumpFiles

getPhotographee :: Message
getPhotographee = GetPhotographee
