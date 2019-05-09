module Elements
    ( addStyleSheet
    , mkSection
    , mkButton
    , mkLabel
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import System.FilePath

import Control.Monad


addStyleSheet :: Window -> FilePath -> FilePath -> UI ()
addStyleSheet w root filename = void $ do
    bulma <- loadFile "text/css" (root </> filename)
    el <- mkElement "link"
            # set (attr "rel" ) "stylesheet"
            # set (attr "type") "text/css"
            # set (attr "href") bulma
    getHead w #+ [element el]


mkSection :: [UI Element] -> UI Element
mkSection xs =
    UI.div #. "section" #+ 
        [UI.div #. "container is-fluid" #+ xs]


mkButton :: String -> UI (Element, Element)
mkButton x = do
    button <- UI.button #. "button" #+ [string x]
    view <- UI.div #. "control" #+ [element button]
    return (button, view)


mkLabel :: String -> UI Element
mkLabel s =
    UI.p #. "has-text-info has-text-weight-bold is-size-5" # set UI.text s
