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
    ) where


import PhotoShake.State (States)

import Control.Concurrent.MVar

import PhotoShake.Dump
import PhotoShake.Doneshooting
import PhotoShake.Dagsdato

data Message 
    = Block (MVar ())

    | GetStates
    | SetStates States

    | GetDump
    | SetDump Dump

    | GetDoneshooting
    | SetDoneshooting Doneshooting

    | GetDagsdato
    | SetDagsdato Dagsdato


block :: MVar () -> Message
block = Block

getStates :: Message
getStates = GetStates

setStates :: States -> Message
setStates = SetStates


getDump :: Message
getDump = GetDump

setDump :: Dump -> Message
setDump = SetDump


getDoneshooting :: Message
getDoneshooting = GetDoneshooting

setDoneshooting :: Doneshooting -> Message
setDoneshooting = SetDoneshooting


getDagsdato :: Message
getDagsdato = GetDagsdato

setDagsdato :: Dagsdato -> Message
setDagsdato = SetDagsdato
