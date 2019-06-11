{-# LANGUAGE OverloadedStrings #-}

import Test.WebDriver
import Control.Monad.Base
import Test.WebDriver.Commands.Wait

import Control.Concurrent.Async
import Lib

import PhotoShake.ShakeConfig
import PhotoShake.Dump

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

runSessionThenClose :: WD a -> IO a
runSessionThenClose action = runSession chromeConfig . finallyClose $ action

main :: IO ()
main = do
    --config <- toShakeConfig (Just "") "test/config.cfg"    
    -- dangerous difference between these params
    race_ (setup 9000 "" "test" "config.cfg")
        (runSessionThenClose $ do                      
            _ <- setImplicitWait 10000
            openPage "http://localhost:9000"
            _ <- setImplicitWait 10000
            tabDump <- findElem ( ById "tabDump" ) 
            --_ <- click tabDump
            --_ <- liftBase $ setDump config $ Dump "lolman"
            


            searchInput <- waitUntil 10000 $ findElem ( ByCSS "input[type='text']" )  
            sendKeys "1234" searchInput
            mover <- findElem ( ById "mover" )
            click mover
            _ <- setImplicitWait 10000
            msg <- findElem ( ById "result" )
            result <- getText msg
            expect (result == "Byg færdigt")
        )

