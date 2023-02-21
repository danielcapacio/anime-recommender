{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module InitializeDatabase where

import           Network.HTTP.Simple
import           Data.Aeson
import           Control.Applicative
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Control.Monad.IO.Class  (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Title
    name String
    deriving Show
Genres
    name String
    title TitleId
    deriving Show
Themes
    name String
    title TitleId
    deriving Show
Studios
    name String
    title TitleId
    deriving Show
Demographics
    name String
    title TitleId
    deriving Show
|]

run :: IO ()
run = do
    response <- httpLbs "https://api.jikan.moe/v4/top/anime?limit=10"
    let body = getResponseBody response
    runSqlite ":memory:" $ do

        runMigration migrateAll
        fmaId <- insert $ Title "Fullmetal Alchemist" 

        insert $ Genres "Shounen" fmaId
        insert $ Genres "Action" fmaId

        oneGenres <- selectList [GenresTitle ==. fmaId] [LimitTo 1]
        liftIO $ print (oneGenres :: [Entity Genres])

        fma <- get fmaId
        liftIO $ print (fma :: Maybe Title)

    print ("123")

