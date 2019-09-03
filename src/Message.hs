module Message
    ( Message(..) -- bad
    , block
    , getStates
    , setStates
    , getDump
    ) where


import State (States)

import Control.Concurrent.MVar

data Message 
    = Block (MVar ())
    | GetStates
    | SetStates States

    | GetDump


block :: MVar () -> Message
block = Block

getStates :: Message
getStates = GetStates

setStates :: States -> Message
setStates = SetStates


getDump :: Message
getDump = GetDump
