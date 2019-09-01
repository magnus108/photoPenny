module Message
    ( Message(..) -- bad
    , configChange
    , getState
    ) where


data Message 
    = Unit
    | ConfigChange
    | GetState


configChange :: Message
configChange = ConfigChange


getState :: Message
getState = GetState
