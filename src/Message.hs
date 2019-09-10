module Message
    ( Message(..) -- bad
    , block
    , getStates
    , setStates
    , getDump
    , setDump
    ) where


import PhotoShake.State (States)

import Control.Concurrent.MVar

import PhotoShake.Dump

data Message 
    = Block (MVar ())

    | GetStates
    | SetStates States

    | GetDump
    | SetDump Dump


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
