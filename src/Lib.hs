{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


import PhotoShake
import Photographee
import PhotoShake.ShakeConfig



import Development.Shake
import Control.Exception

--import System.FSNotify hiding (defaultConfig)
--import Control.Concurrent 



someFunc :: Int -> IO ()
someFunc port = do
    config <- try $ toShakeConfig "config.cfg" :: IO (Either SomeException ShakeConfig)
    case config of 
        Right c -> 
            startGUI
                defaultConfig { jsStatic = Just "static"
                              , jsPort = Just port
                              } (setup c)
        Left _ -> 
            startGUI
                defaultConfig { jsStatic = Just "static"
                              , jsPort = Just port
                              } missingConf
            
    --dirs <- listDirectory $ _dumpDir config
   -- withManager $ \mgr -> do
        -- start a watching job (in the background)
     --   msgChan <- newChan
      --  outChan <- newChan
       -- dumpChan <- newChan
       -- locationChan <- newChan
      --  _ <- watchDirChan
        --    mgr
         --   "/home/magnus/Documents/projects/photoPenny/out"
          --  (const True)
           -- outChan

       -- _ <- watchDirChan
        --    mgr
         --   "/home/magnus/Documents/projects/photoPenny/dump"
        --    (\x -> case x of
          --      Added _ _ _ -> True
           --     _ -> False)
       --     dumpChan
        
     --   _ <- watchDirChan
      --      mgr
       --     "/home/magnus/Documents/projects/photoPenny/locations"
        --    (const True)
         --   locationChan


mkButton :: String -> String -> UI (Element, Element)
mkButton titl ident = do
    button <- UI.button #. "button" # set UI.id_ ident #+ [string titl]
    view <- UI.div #. "control" #+ [element button]
    return (button, view)


mkSection :: [UI Element] -> UI Element
mkSection xs =
    UI.div #. "section" #+ 
        [UI.div #. "container is-fluid" #+ xs]

mkLabel :: String -> UI Element
mkLabel s =
    UI.p #. "has-text-info has-text-weight-bold is-size-5" # set UI.text s


--mkInput :: Chan String -> String -> UI (Element, Element)
--mkInput chan s = do
--    input <- UI.input #. "input" # set UI.type_ "text" 
--    view <- UI.div #. "field is-horizontal" #+
--        [ UI.div #. "field-label is-normal" #+
--            [ UI.label #. "label" # set UI.text s ]
--        , UI.div #. "field-body" #+
--            [ UI.div #. "field" #+
--                [ UI.p #. "control" #+ [ element input ] ]
--            ]
--        ]

--    on UI.keyup input $ \_ -> do
--        val <- get value input
--        liftIO $ writeChan chan val
        
--    return (input, view)


missingConf :: Window -> UI ()
missingConf w = do
    _ <- UI.addStyleSheet w "bulma.min.css"

    (_, view) <- mkButton "Run build" "thisId"
    (_, view1) <- mkButton "Select folder" "thisId1"
 
    dumpChanges <- UI.p

    dump <- mkSection
        [ mkLabel "DumpDir"
        --, UI.p # set UI.text (_dumpDir config)
        --, UI.p # set UI.text (head dumps)
        , element dumpChanges
        ]

    out <- mkSection
        [ mkLabel "OutDir"
        --, UI.p # set UI.text (_outDir config)
        ]

    locationChanges <- UI.select

    location <- mkSection
        [ mkLabel "Location"
        --, UI.p # set UI.text (_location config)
        , UI.div #. "field" #+ 
            [ UI.div #. "control" #+
                [ UI.div #. "select" #+ 
                    [ element locationChanges
                    ]
                ]
            ]
        ]

    --(_, inputView) <- mkInput msgChan "id" 
    
    ident <- UI.p

    photoConfig <- mkSection 
        [ mkLabel "PhotographeeId"
        , element ident
        --, element inputView
        ]

    msg <- UI.p
    section <- mkSection [
             element view1
            , element view
            , element msg
            , element dump
            , element out
            , element location
            , element photoConfig
            ]

    _ <- getBody w #+ [element section] 
         
    return ()
    
--setup :: ShakeConfig -> [FilePath] -> EventChannel -> EventChannel -> Chan String -> Window -> UI ()
--setup config dumps dumpChan locationChan msgChan w = do
setup :: ShakeConfig -> Window -> UI ()
setup config w = do
    _ <- UI.addStyleSheet w "bulma.min.css"

    (_, view) <- mkButton "Run build" "thisId"
    (button1, view1) <- mkButton "Select folder" "thisId1"
 
    dumpChanges <- UI.p

    dump <- mkSection
        [ mkLabel "DumpDir"
        --, UI.p # set UI.text (_dumpDir config)
        --, UI.p # set UI.text (head dumps)
        , element dumpChanges
        ]

    out <- mkSection
        [ mkLabel "OutDir"
        , UI.p # set UI.text (_outDir config)
        ]

    locationChanges <- UI.select

    location <- mkSection
        [ mkLabel "Location"
        , UI.p # set UI.text (_location config)
        , UI.div #. "field" #+ 
            [ UI.div #. "control" #+
                [ UI.div #. "select" #+ 
                    [ element locationChanges
                    ]
                ]
            ]
        ]

    --(_, inputView) <- mkInput msgChan "id" 
    
    ident <- UI.p

    photoConfig <- mkSection 
        [ mkLabel "PhotographeeId"
        , element ident
        --, element inputView
        ]

    msg <- UI.p
    section <- mkSection [
             element view1
            , element view
            , element msg
            , element dump
            , element out
            , element location
            , element photoConfig
            ]

    _ <- getBody w #+ [element section] 
         
    _ <- onElementId "thisId" "click" config (Photographee "lol" "lol" "lol")
        (element msg # set text "Clicked")
        msg
     --
     --
    --selectFolder "thisId1" "click" $ \x -> do
    --    putStrLn "lol"
    --    return ()
    --on UI.click button1 $ \_ -> do
    --    runFunction $ ffi "require('electron').remote.dialog.showOpenDialog({properties: ['openDirectory']}, (folder) => { console.log(folder)})"
    
    on UI.click button1 $ \_ -> do
        callback <- ffiExport $ \folder -> do
            putStrLn folder
            return ()

        runFunction $ ffi "require('electron').remote.dialog.showOpenDialog({properties: ['openDirectory']}, %1)" callback


    --dumpChan' <- liftIO $ dupChan dumpChan
    --void $ liftIO $ forkIO $ receiveDumps w dumpChan' dumpChanges

    --locationChan' <- liftIO $ dupChan locationChan
    --void $ liftIO $ forkIO $ receiveLocations w locationChan' locationChanges

    --msgChan' <- liftIO $ dupChan msgChan
    --void $ liftIO $ forkIO $ receiveMsg w msgChan' ident
    
    --
    --on UI.click button $ \_ -> do 
      --  runFunction $ ffi "require('electron').shell.openItem('/home/magnus/Downloads/fetmule.jpg')"

{-
receiveDumps :: Window -> EventChannel -> Element -> IO ()
receiveDumps w events dumpFiles = do
    messages <- getChanContents events
    forM_ messages $ \msg -> do
        runUI w $ do
          _ <- element dumpFiles #+ [UI.p # set UI.text (takeFileName $ eventPath msg)]
          flushCallBuffer

receiveLocations :: Window -> EventChannel -> Element -> IO ()
receiveLocations w events locationFiles = do
    messages <- getChanContents events
    forM_ messages $ \msg -> do
        runUI w $ do
          _ <- element locationFiles #+ [UI.option # set UI.text (takeFileName $ eventPath msg)]
          flushCallBuffer

receiveMsg :: Window -> Chan String -> Element -> IO ()
receiveMsg w events ident = do
    messages <- getChanContents events
    forM_ messages $ \msg -> do
        runUI w $ do
          _ <- element ident # set UI.text msg
          flushCallBuffer
-}

--selectFolder :: String -> String -> (FilePath -> IO ()) -> UI ()
--selectFolder elid event complete = do
--    callback <- ffiExport complete
--    runFunction $ ffi "$(%1).on(%2,require('electron').remote.dialog.showOpenDialog({properties: ['openDirectory']}, %3))" ("#"++elid) event callback


onElementId :: String -> String -> ShakeConfig -> Photographee -> UI void -> Element -> UI ()
onElementId elid event config photographee handler err = do
    window   <- askWindow
    exported <- ffiExport $ do
        shakeIO <- try $ myShake config photographee :: IO (Either ShakeException ())
        case shakeIO of
            Left x -> runUI window 
                (element err # set text (displayException (shakeExceptionInner x))) >> return ()
            Right _ -> runUI window handler >> return ()
   
    runFunction $ ffi "$(%1).on(%2,%3)" ("#"++elid) event exported


