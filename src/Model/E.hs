{-# LANGUAGE OverloadedStrings #-}
module Model.E
    ( E
    , production
    , test
    , unApp
    , App

    , _configs 
    ) where

import Utils.Env
import Utils.Comonad



data E = Production | Test

production :: E
production = Production

test :: E
test = Test

newtype App a = App { unApp :: Env E a }






_configs :: App FilePath -> FilePath
_configs = extract . unApp
