{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}

module InitializeDatabase where

import           Network.HTTP.Simple
import           Data.Aeson
import           Control.Applicative
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad (mzero, forM_)
import           GHC.Generics

{-
 - Defining the parsing behaviour
 - of the JSON that will be fetched
 -}
data StudiosParsed = StudiosParsed 
    { nameStudio:: String
    } deriving (Show)

data GenresParsed = GenresParsed 
    { nameGenre:: String
    } deriving (Show)

data ThemesParsed = ThemesParsed
    { nameTheme:: String
    } deriving (Show)

data DemographicsParsed = DemographicsParsed
    { nameDemographic:: String
    } deriving (Show)

data Info = Info 
    { title_english  :: String
    , studios :: [StudiosParsed]
    , genres   :: [GenresParsed]
    , themes        :: [ThemesParsed]
    , demographics :: [DemographicsParsed]
    } deriving (Show)

data Dataset = Dataset
    { dataObj :: [Info]
    } deriving (Show)

instance FromJSON Dataset where
    parseJSON (Object o) = 
        Dataset <$> o .: "data"
    parseJSON _ = mzero

instance FromJSON Info where
    parseJSON (Object o) = 
        Info <$> o .: "title_english"
        <*> o .: "studios"
        <*> o .: "genres"
        <*> o .: "themes"
        <*> o .: "demographics"
    parseJSON _ = mzero

instance FromJSON StudiosParsed where
    parseJSON (Object o) =
        StudiosParsed <$> o .: "name"
    parseJSON _ = mzero

instance FromJSON GenresParsed where
    parseJSON (Object o) =
        GenresParsed <$> o .: "name"
    parseJSON _ = mzero

instance FromJSON ThemesParsed where
    parseJSON (Object o) =
        ThemesParsed <$> o .: "name"
    parseJSON _ = mzero

instance FromJSON DemographicsParsed where
    parseJSON (Object o) =
        DemographicsParsed <$> o .: "name"
    parseJSON _ = mzero

{-
 - Defining the Database Tables:
 - Title contains:
 - TitleId
 - Name
 -
 - Studios/Genres/Themes/Demographics contains:
 - StudiosId/GenresId/ThemesId/DemographicsId
 - Name
 - TitleId
 -
 -}
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Title
    name String
    deriving Show
Studios
    name String
    title TitleId
    deriving Show
Genres
    name String
    title TitleId
    deriving Show
Themes
    name String
    title TitleId
    deriving Show
Demographics
    name String
    title TitleId
    deriving Show
|]

{-
 - Fetches response from API, parses it, and places the results in the database
 -}
run :: IO ()
run = do
    response <- httpLbs "https://api.jikan.moe/v4/top/anime?limit=10"
    let body = getResponseBody response
    let decoded = eitherDecode body :: Either String Dataset

    runSqlite ":memory:" $ do
        runMigration migrateAll

        case decoded of
            Left o -> 
                error "error: API response did not return correctly"
            Right o -> 
                forM_ (dataObj o) $ \i -> do
                    tempId <- insert $ Title (title_english i)
                    -- liftIO $ print (title_english i)
                    forM_ (studios i) $ \s -> do
                        insert $ Studios (nameStudio s) tempId
                        -- liftIO $ print (nameStudio s)
                    forM_ (genres i) $ \g -> do
                        insert $ Genres (nameGenre g) tempId
                        -- liftIO $ print (nameGenre g)
                    forM_ (themes i) $ \t -> do
                        insert $ Themes (nameTheme t) tempId
                        -- liftIO $ print (nameTheme t)
                    forM_ (demographics i) $ \d -> do
                        insert $ Demographics (nameDemographic d) tempId
                        -- liftIO $ print (nameDemographic d)
        
        -- get all shows with the same genre as the FMA
        title1 <- selectKeysList [TitleName ==. "Fullmetal Alchemist: Brotherhood"] [LimitTo 1]
        genre <- selectList [GenresTitle ==. (head title1)] []
        forM_ genre $ \g -> do
            let var = entityVal g
            title <- selectList [GenresName ==. genresName var] []
            forM_ title $ \t -> do
                let show = entityVal t
                titles <- selectList [TitleId ==. genresTitle show] []
                forM_ titles $ \name -> do
                    let a = entityVal name
                    liftIO $ print (titleName a)

