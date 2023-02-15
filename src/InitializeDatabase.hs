{-# LANGUAGE OverloadedStrings #-}
module InitializeDatabase where

import Network.HTTP.Simple
import Data.Aeson

get :: IO ()
get = do
    response <- httpLbs "https://api.jikan.moe/v4/top/anime?limit=10"
    print (getResponseBody response)

