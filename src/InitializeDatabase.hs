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
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad (mzero, forM_, forM, foldM, filterM)
import           GHC.Generics
import           Data.List (nub)
import           System.Directory (removeFile)

{-
 - Defining the parsing behaviour
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
    { title :: String
    , image_url :: String
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
        Info <$> o .: "title"
        <*> (o .: "images" >>= (.: "jpg") >>= (.: "image_url"))
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
    url String
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

fetchPage :: String -> IO(Either String Dataset)
fetchPage page = do
    let requestUrl = "https://api.jikan.moe/v4/top/anime" ++ "?page=" ++ page :: String
    request <- parseRequest requestUrl
    response <- httpLbs request
    let body = getResponseBody response
    let decoded = eitherDecode body :: Either String Dataset
    return decoded

{-
 - Fetches response from API, parses it, and places the results in the database
 -}
loadData :: IO ()
loadData = do
    page1 <- fetchPage "1"
    page2 <- fetchPage "2"
    page3 <- fetchPage "3"
    page4 <- fetchPage "4"
    let pages = [page1, page2, page3, page4] :: [Either String Dataset]

    removeFile "database.sqlite3"
    runSqlite "database.sqlite3" $ do
        runMigration migrateAll

        forM_ (pages) $ \page -> do
            case (page) of
               Left o -> 
                   error o
               Right o -> 
                   forM_ (dataObj o) $ \i -> do
                       tempId <- insert $ Title (title i) (image_url i)
                       forM_ (studios i) $ \s -> do
                           insert $ Studios (nameStudio s) tempId
                       forM_ (genres i) $ \g -> do
                           insert $ Genres (nameGenre g) tempId
                       forM_ (themes i) $ \t -> do
                           insert $ Themes (nameTheme t) tempId
                       forM_ (demographics i) $ \d -> do
                           insert $ Demographics (nameDemographic d) tempId
      
{- 
 - Input: A string containing the title of a show
 - Returns: All the titles of shows in the database that share the same genre as the input 
 - If the title appears multiple times, it means the show shares multiple genres with the input
 -}
getGenre :: String -> IO [String]
getGenre inputTitle = do
    runSqlite "database.sqlite3" $ do
        -- get all shows with the same genre as the FMA
        title1 <- selectKeysList [TitleName ==. inputTitle] [LimitTo 1]
        genreList <- selectList [GenresTitle ==. (head title1)] [] -- list of genres of given show
        let sameGenreAccumulator = [] :: [String]
        
        -- loop through all the genres in given show
        result <- foldM (\acc genre -> do
            let genreEntity = entityVal genre
            showList <- selectList [GenresName ==. genresName genreEntity] [] -- list of shows with same genre as genreName
            let middleList = [] :: [String]
            -- loop through all the shows in the [genreName] genre, add their names to the middleResult list
            middleResult <- foldM (\ac show -> do
                let showEntity = entityVal show
                title <- selectList [TitleId ==. genresTitle showEntity] [LimitTo 1]
                let titleEntity = entityVal (head title)
                return (ac ++ [titleName titleEntity])) middleList showList
            -- add the middleResult to the total accumulator list
            return (acc ++ middleResult)) sameGenreAccumulator genreList
        result <- filterM (\x -> return $ x /= inputTitle) result
        return result

getGenreShows :: String -> IO [String]
getGenreShows genre = do
    runSqlite "database.sqlite3" $ do
        genreList <- selectList [GenresName ==. genre] []
        result <- foldM (\acc genreEntity -> do
            let genre = entityVal genreEntity
            title <- selectList [TitleId ==. genresTitle genre] [LimitTo 1]
            let titleEntity = entityVal (head title)
            return (acc ++ [titleName titleEntity])) [] genreList
        return result

getTitles :: IO [(String, String)]
getTitles = do
    runSqlite "database.sqlite3" $ do
        titleList <- selectList [] []
        result <- foldM (\acc titleEntity -> do
            let title = entityVal titleEntity
            return (acc ++ [(titleName title, titleUrl title)])) [] titleList
        return result
        
