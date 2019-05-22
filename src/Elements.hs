module Elements
    ( addStyleSheet
    , mkSection
    , mkButton
    , mkLabel
    , mkColumns
    , mkColumn
    , mkInput
    , mkFolderPicker
    , mkFilePicker
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import System.FilePath

import Control.Monad

import Data.List


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
        [ UI.div #. "container is-fluid" #+ xs]


mkButton :: String -> UI (Element, Element)
mkButton x = do
    button <- UI.button #. "button" #+ [string x]
    view <- UI.div #. "control" #+ [element button]
    return (button, view)


mkLabel :: String -> UI Element
mkLabel s =
    UI.p #. "has-text-info has-text-weight-bold is-size-5" # set UI.text s


mkColumns :: [String] -> [UI Element] -> UI Element
mkColumns xs x = UI.div #. classes #+ x
    where
        classes = intercalate " " $ "columns" : xs

mkColumn :: [String] -> [UI Element] -> UI Element
mkColumn xs x = UI.div #. classes #+ x
    where
        classes = intercalate " " $ "column" : xs

mkInput :: String -> UI (Element, Element)
mkInput s = do
    input <- UI.input #. "input" # set UI.type_ "text" 
    view <- UI.div #. "field" #+
        [ UI.label #. "label has-text-info" # set UI.text s
        , UI.div #. "control" #+ [ element input ] 
        ]
    return (input, view)


mkFolderPicker :: String -> (FilePath -> IO ()) -> UI (Element, Element)
mkFolderPicker = mkShowOpenDialog ["openDirectory"]


mkFilePicker :: String -> (FilePath -> IO ()) -> UI (Element, Element)
mkFilePicker = mkShowOpenDialog ["openFile"]


mkShowOpenDialog :: [String] -> String -> (FilePath -> IO ()) -> UI (Element, Element) 
mkShowOpenDialog options x fx = do
    (button, view) <- mkButton x

    on UI.click button $ \_ -> do
        cb <- ffiExport fx
        runFunction $ ffi "require('electron').remote.dialog.showOpenDialog({properties: %2}, %1)" cb options

    return (button, view)
