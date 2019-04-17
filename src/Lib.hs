module Lib
    ( someFunc
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


mkButton :: String -> String -> UI (Element, Element)
mkButton title id = do
    button <- UI.button #. "button" # set UI.id_ id #+ [string title]
    view   <- UI.p #+ [element button]
    return (button, view)


setup :: Window -> UI ()
setup w = do
    return w # set title "App"
    
    (button, view) <- mkButton "Run build" "thisId"

    msg <- UI.span # set UI.text "Some text"

    getBody w #+ [element view, element msg]
    
    body <- getBody w
     
    onElementId "thisId" "click" $ do
        (\b -> if b then element msg # set text "Clicked" else element msg # set text "error")

onElementId
    :: String   -- ID attribute of the element
    -> String   -- name of the DOM event to register the handler at
    -> (Bool -> UI void) -- handler to fire whenever the event happens
    -> UI ()
onElementId elid event handler = do
    window   <- askWindow
    exported <- ffiExport (helloWorld >>= (\b -> runUI window (handler b)) >> return ())
    runFunction $ ffi "$(%1).on(%2,%3)" ("#"++elid) event exported


helloWorld :: IO Bool 
helloWorld = do
    putStrLn "Lol"
    return False


someFunc :: IO ()
someFunc = startGUI defaultConfig setup
