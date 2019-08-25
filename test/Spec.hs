{-# LANGUAGE OverloadedStrings #-}

import Test.WebDriver hiding (setLocation)
import Control.Monad.Base
import Test.WebDriver.Commands.Wait
import Utils.FP

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Lib

import System.Directory


import Test.WebDriver.Common.Keys (enter)


import State

import PhotoShake.ShakeConfig
import PhotoShake.Photographee
import PhotoShake.Built


import Control.Monad

import Utils.ListZipper
import qualified Utils.Actions as A

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

runSessionThenClose :: WD a -> IO a
runSessionThenClose action = runSession chromeConfig . finallyClose $ action

main :: IO ()
main = do
    config <- toShakeConfig Nothing "test/config.cfg"    
    -- dangerous difference between these params
    race_ (setup 9000 "" "test/config.cfg" "test/config" "test/config/state.json")
        (runSessionThenClose $ do                      
            -- copy in pictures
            fmap and $  forM [1..40] (\iter -> do
                openPage "http://localhost:9000"

                _ <- liftBase $ A.interpret $ setStates (mkFP "" "test/config/state.json") (States $ ListZipper [Session,Shooting,Doneshooting,Dagsdato,Dump,Photographer] Location [Main, Control])

                _ <- liftBase $ A.interpret $ setStates (mkFP "" "test/config/state.json") (States $ ListZipper [Location, Session,Shooting,Doneshooting,Dagsdato,Dump,Photographer] Main [Control])

                {-
                forM_ [1..6] (\x -> do
                    liftBase $ threadDelay 1000000
                    waitUntil 10000000 $ findElem ( ById "tabDump" ) >>= click
                    liftBase $ threadDelay 1000000
                    waitUntil 1000000 $ findElem ( ById "tabPhotographer" ) >>= click
                    liftBase $ threadDelay 1000000
                    waitUntil 1000000 $ findElem ( ById "tabDoneshooting" ) >>= click
                    liftBase $ threadDelay 1000000
                    waitUntil 1000000 $ findElem ( ById "tabShooting" ) >>= click
                    liftBase $ threadDelay 1000000
                    waitUntil 1000000 $ findElem ( ById "tabLocation" ) >>= click
                    liftBase $ threadDelay 1000000
                    waitUntil 1000000 $ findElem ( ById "tabMain" ) >>= click
                    )
                -}

                liftBase $ forM_ [1..40] (\x -> do
                    writeFile ("test/files/input" ++ (show x) ++ ".CR2") (show [1..8000000]) -- create the source file
                    writeFile ("test/files/input" ++ (show x) ++ ".JPG") (show [1..8000000])-- create the source file
                    )

                liftBase $ setIdSelection config (Idd "")
                liftBase $ setBuilt' config (NoBuilt)

                liftBase $ threadDelay 4000000
                searchInput <- waitUntil 100000 $ findElem ( ById "fotoId" )
                sendKeys "1234" searchInput

                liftBase $ threadDelay 400000 -- vent for text alfredvestved
                --mover <- waitUntil 100000 $ findElem ( ById "builderButton" )
                --click mover
                searchInput2 <- waitUntil 10000 $ findElem ( ById "fotoId" )
                sendKeys enter searchInput2 

                liftBase $ putStrLn "gg2"
                liftBase $ threadDelay 30000000
                liftBase $ putStrLn "gg"
                liftBase $ threadDelay 100000
                liftBase $ putStrLn "gg"
                --msg <- waitUntil 1000000 $ findElem ( ById "result" )
                --result <- getText msg
                --expect (result == "Færdig")

                --WWHAT???
                --liftBase $ removeDirectoryRecursive "/home/magnus/Documents/projects/photoPenny/test/doneshooting/"
                --liftBase $ removeDirectoryRecursive "/home/magnus/Documents/projects/photoPenny/test/dagsdato/"


                return True
                )
        )
        {-
            _ <- liftBase $ setDump config $ NoDump -- reset or i will get stales
            _ <- liftBase $ setDagsdato config $ NoDagsdato -- reset or i will get stales
            _ <- liftBase $ setPhotographers config $ NoPhotographers -- reset or i will get stales
            _ <- liftBase $ setDoneshooting config $ NoDoneshooting -- reset or i will get stales
            _ <- liftBase $ setDagsdato config $ NoDagsdato -- reset or i will get stales
            _ <- liftBase $ setShooting config $ NoShootings -- reset or i will get stales
            _ <- liftBase $ setSession config $ NoSessions -- reset or i will get stales
            _ <- liftBase $ setLocation config $ NoLocation -- reset or i will get stales

            openPage "http://localhost:9000"
            --DUMP
            --DUMP
            tabDump <- waitUntil 100000 $ findElem ( ById "tabDump" ) 
            _ <- click tabDump
            _ <- liftBase $ setDump config $ Dump "testMappe" -- maybe i should inject this service
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "dumpOK")

            --DAGSDATO
            --DAGSDATO
            tabDagsdato <- waitUntil 100000 $ findElem ( ById "tabDagsdato" ) 
            _ <- click tabDagsdato
            _ <- liftBase $ setDagsdato config $ Dagsdato "testMappe" -- maybe i should inject this service
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "dagsdatoOK")

            --PHOTOGRAPHER
            --PHOTOGRAPHER
            tabPhotographer <- waitUntil 100000 $ findElem ( ById "tabPhotographer" ) 
            _ <- click tabPhotographer
            _ <- liftBase $ setPhotographers config $ Photographers (ListZipper [] (Photographer "Joe Test" "JT") []) -- maybe i should inject this service
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "photographerOK")
            
            --DONESHOOTING
            --DONESHOOTING
            tabDoneshooting <- waitUntil 100000 $ findElem ( ById "tabDoneshooting" ) 
            _ <- click tabDoneshooting
            _ <- liftBase $ setDoneshooting config $ Doneshooting "testMappe"
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "doneshootingOK")

            --SHOOTING
            --SHOOTING
            tabShooting <- waitUntil 100000 $ findElem ( ById "tabShooting" ) 
            _ <- click tabShooting
            _ <- liftBase $ setShooting config $ Shootings  (ListZipper [] Normal [])
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "shootingOK")
            
            --SESSION
            --SESSION
            tabSession <- waitUntil 100000 $ findElem ( ById "tabSession" ) 
            _ <- click tabSession
            _ <- liftBase $ setSession config $ Sessions (ListZipper [] School [])
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "sessionOK")

            --LOCATION
            --LOCATION
            tabLocation <- waitUntil 100000 $ findElem ( ById "tabLocation" ) 
            _ <- click tabLocation
            _ <- liftBase $ setLocation config $ Location "testlocation.csv"
            -- Der burde nok være en gaurd på stale elements men ok..
            _ <- waitUntil 100000 $ findElem (ById "locationOK")



            tabMain <- waitUntil 100000 $ findElem ( ById "tabMain" ) 
            _ <- click tabMain 
            searchInput <- waitUntil 10000 $ findElem ( ByCSS "input[type='text']" )  
            sendKeys "1234" searchInput
            mover <- waitUntil 100000 $ findElem ( ById "mover" )
            click mover
            msg <- findElem ( ById "result" )
            result <- getText msg
            expect (result == "Byg færdigt")
            _ <- liftBase $ putStrLn "complete"
            expect True
        )
        -}
