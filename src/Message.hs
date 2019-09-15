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
    ) where


import PhotoShake.State (States)

import Control.Concurrent.MVar

import qualified PhotoShake.Dump as Dump
import qualified PhotoShake.Doneshooting as Doneshooting 
import qualified PhotoShake.Dagsdato as Dagsdato
import qualified PhotoShake.Photographer as Photographer

data Message 
    = Block (MVar ())

    | GetStates
    | SetStates States

    | GetDump
    | SetDump Dump.Dump

    | GetDoneshooting
    | SetDoneshooting Doneshooting.Doneshooting

    | GetDagsdato
    | SetDagsdato Dagsdato.Dagsdato

    | GetPhotographers
    | SetPhotographers Photographer.Photographers

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
