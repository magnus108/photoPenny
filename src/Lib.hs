{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Elements

--import PhotoShake
--import Photographee
--import PhotoShake.ShakeConfig

--import Development.Shake.FilePath

--import Control.Monad 

--import Control.Exception
--import Development.Shake

--import System.FSNotify hiding (defaultConfig)
--import Control.Concurrent
--import Data.IORef

--import Elements


someFunc :: Int -> String -> IO ()
someFunc port root = do
    -- opret config hvis den ikke findes
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
    startGUI
        defaultConfig { jsPort = Just port
                      } (view root)

            
view :: FilePath -> Window -> UI ()
view root w = do
    _ <- addStyleSheet w root "bulma.min.css"
    section <- mkSection [ UI.p # set UI.text "Mangler måske config" ]
    _ <- getBody w #+ [element section] 
    return ()


---mkBuild :: IORef ShakeConfig -> FilePath -> IORef String -> Window -> Element -> Element -> UI (Element, Element)
--mkBuild config root idd w err msg = do
   -- (button, view) <- mkButton "Kør byg"
   -- callback <- ffiExport $ funci config root idd w err msg
    --runFunction $ ffi "$(%1).on('click',%2)" button callback
   -- return (button, view)


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
    

--funci :: (IORef ShakeConfig) -> FilePath -> (IORef String) -> Window -> IO ()
--funci config root idd w err msg = do
    -- have to look this up from config
  --  idd2 <- readIORef idd
  --  conf <- readIORef config
   -- photographee <- findPhotographee (root </> _location conf) idd2
  --  build <- try $ myShake conf photographee :: IO (Either ShakeException ())
    --let ans = case build of
    --        Left _ -> element err # set text "Der skete en fejl"  
    --        Right _ -> element msg # set text "Byg færdigt"
   -- _ <- runUI w ans
   -- return ()
