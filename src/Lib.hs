{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( setup
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

import PhotoShake
import PhotoShake.ShakeConfig
import PhotoShake.Photographee

import System.FilePath

--import Development.Shake.FilePath

--import Control.Monad 

import Control.Exception
import Development.Shake

--import System.FSNotify hiding (defaultConfig)
--import Control.Concurrent
import Data.IORef

--import Elements


setup :: Int -> String -> IO ()
setup port root = do
    {-
    config <- try $ toShakeConfig "config.cfg" :: IO (Either SomeException ShakeConfig)
    withManager $ \mgr -> do
            msgChan <- newChan
            _ <- watchDirChan
                    mgr
                    "."
                   (\x -> takeFileName (eventPath x) == "config.cfg") -- need a better check
                    msgChan

            view <- case config of 
                    Right c -> do
                        conf <- newIORef c
                        return $ setup conf msgChan
                    Left _ -> return missingConf
            startGUI
                defaultConfig { jsPort = Just port
                              } (view root)
        -}
    config <- toShakeConfig "config.cfg" -- can throw error
    startGUI
        defaultConfig { jsPort = Just port
                      } (main root config)

            
main :: FilePath -> ShakeConfig -> Window -> UI ()
main root shakeConfig w = do
    _ <- addStyleSheet w root "bulma.min.css"
    _ <- body w root shakeConfig
    return ()


body :: Window -> FilePath -> ShakeConfig -> UI ()
body w root config = do
    --section <- mkSection [ UI.p # set UI.text "Mangler måske config" ]
    
    --nicess
    let lol = _dumpConfig config 
    dumpConfig <- mkSection [ UI.p # set UI.text lol
                            , mkConfPicker root lol
                            ]

    let lol1 = _dagsdatoConfig config 
    dagsdatoConfig <- mkSection [ UI.p # set UI.text lol1
                            , mkConfPicker root lol1
                            ]

    let lol2 = _doneshootingConfig config 
    doneshootingConfig <- mkSection [ UI.p # set UI.text lol2
                                    , mkConfPicker root lol2
                                    ]

    ident <- liftIO $ newIORef ""
    (_, buildView) <- mkBuild config root ident w

    (input, inputView) <- mkInput "elev nr:"
    on UI.keyup input $ \_ -> liftIO . writeIORef ident =<< get value input

    _ <- getBody w #+ [element dumpConfig, element dagsdatoConfig , element doneshootingConfig, element buildView, element inputView]
    return ()


mkConfPicker :: FilePath -> FilePath -> UI Element
mkConfPicker _ conf = do
    (_, view) <- mkFolderPicker "Vælg config folder" $ \folder -> do
        --this is full path will
        --that matter?
        writeFile conf $ "location = " ++ folder
        return ()
    return view


--mkBuild config root idd w err msg = do
mkBuild :: ShakeConfig -> FilePath -> IORef String -> Window -> UI (Element, Element)
mkBuild config root idd w = do
    (button, view) <- mkButton "Kør byg"
    callback <- ffiExport $ funci config root idd w 
    runFunction $ ffi "$(%1).on('click',%2)" button callback
    return (button, view)


{-
mkContent :: UI Element
mkContent = do
    mkColumns ["is-multiline"]
        [ mkColumn ["is-12"] [ element view ]
        , mkColumn ["is-12"] [ mkOutDirPicker ]
        , mkColumn ["is-8"] [ element inputView ]
        , mkColumn ["is-12"] [ element msgChanges ]
        , mkColumn ["is-12"] [ element view2 ]
        ]


mkOutDirPicker :: UI Element
mkOutDirPicker = do
    (_, view) <- mkFolderPicker "Vælg config folder"
    return view
-}

--setup :: IORef ShakeConfig -> EventChannel -> String -> Window -> UI ()
--setup config msgChan root w = do
  --  _ <- addStyleSheet w root "bulma.min.css"

  --  ident <- liftIO $ newIORef ""
--    (_, view) <- mkBuild config root ident w

--    (input, inputView) <- mkInput "elev nr:"
--    on UI.keyup input $ \_ -> liftIO . writeIORef ident =<< get value input

 --   _ <- getBody w #+ [ mkSection [ mkContent ]] 

    --return () 


--missingConf :: FilePath -> Window -> UI ()
--missingConf root w = do
--    _ <- addStyleSheet w root "bulma.min.css"
--    section <- mkSection [UI.p # set UI.text "Mangler måske config"]
--    _ <- getBody w #+ [element section] 
--    return ()
    

--funci config root idd w err msg = do
funci :: ShakeConfig -> FilePath -> (IORef String) -> Window -> IO ()
funci config root idd _ = do
    --have to look this up from config
    idd2 <- readIORef idd
    photographee <- findPhotographee (root </> _location config) idd2
    _ <- try $ myShake config photographee :: IO (Either ShakeException ())
    --let ans = case build of
    --        Left _ -> element err # set text "Der skete en fejl"  
    --        Right _ -> element msg # set text "Byg færdigt"
    --_ <- runUI w ans
    return ()
