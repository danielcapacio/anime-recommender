{-# LANGUAGE OverloadedStrings          #-} -- required for API call
{-# LANGUAGE QuasiQuotes                #-} -- required for database
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DerivingStrategies         #-} 
{-# LANGUAGE StandaloneDeriving         #-} 
{-# LANGUAGE UndecidableInstances       #-} 
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}

module Database where

import           Network.HTTP.Simple
import           Data.Aeson
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad (mzero, forM_, forM, foldM, filterM)
import           Data.List (nub)
import           System.Directory (removeFile)

{-
 - Defining the types in which we store the parsed JSON
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

{-
 - Defining the parsing behaviour
 -}
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
 - Url
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

{-
 - Fetches response from API, parses it, and returns it
 -}
fetchPage :: String -> IO(Either String Dataset)
fetchPage page = do
    let requestUrl = "https://api.jikan.moe/v4/top/anime" ++ "?page=" ++ page :: String
    request <- parseRequest requestUrl
    response <- httpLbs request
    let body = getResponseBody response
    let decoded = eitherDecode body :: Either String Dataset
    return decoded

{-
 - Places the response from the API to the database
 -}
loadData :: IO ()
loadData = do
    page1 <- fetchPage "1"
    page2 <- fetchPage "2"
    page3 <- fetchPage "3"
    page4 <- fetchPage "4"
    let pages = [page1, page2, page3, page4] :: [Either String Dataset]

    removeFile "database.sqlite3" -- remove the previous database before creating a new one
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
 - Input: A string of a show defined in the database
 - Returns: A list of strings containing all shows that have a common genre with the given string
 -}
getGenres :: String -> IO [String]
getGenres inputAnime = getField GenresTitle GenresName genresName genresTitle inputAnime

{-
 - Input: A string of a show defined in the database
 - Returns: A list of strings containing all shows that have a common theme with the given string
 -}
getThemes :: String -> IO [String]
getThemes inputAnime = getField ThemesTitle ThemesName themesName themesTitle inputAnime

{-
 - input: a string of a show defined in the database
 - returns: a list of strings containing all shows that have a common demographic with the given string
 -}
getDemographics :: String -> IO [String]
getDemographics inputAnime = getField DemographicsTitle DemographicsName demographicsName demographicsTitle inputAnime

{-
 - input: a string of a show defined in the database
 - returns: a list of strings containing all shows that have a common studio with the given string
 -}
getStudios :: String -> IO [String]
getStudios inputAnime = getField StudiosTitle StudiosName studiosName studiosTitle inputAnime

{-
 - Input: An EntityField containing the title and the name, two functions that retrieve the name and the title respectively given a Genres/Themes/Studios/Demographics Entity, and a string of a show defined in the database.
 - Returns: A function that searches the database for every show that has a common Genres/Themes/Studio/Demographics (depending on the input) with the given string
 -}
getField titleField nameField nameFun titleFun inputAnime = do
    runSqlite "database.sqlite3" $ do
        -- Grab the Key of the Input Anime
        inputAnimeKey <- selectKeysList [TitleName ==. inputAnime] [LimitTo 1]

        -- Grab all the fields of the inputAnime and store it in "fieldList"
        fieldList <- selectList [titleField ==. head inputAnimeKey] [] 

        -- fold through "fieldList" and create "result" that stores list of anime titles that share a field

        result <- foldM (\ acc field -> do -- search through database and collect all animes with the field
            matchingGenreAnimes <- selectList [nameField ==. nameFun (entityVal field)] []
            let middleList = [] :: [String]
            -- loop through all the shows in the [fieldName] field, add their names to the middleResult list
            middleResult <- foldM (\ac show -> do
                let showEntity = entityVal show
                title <- selectList [TitleId ==. titleFun showEntity] [LimitTo 1]
                let titleEntity = entityVal (head title)
                return (ac ++ [titleName titleEntity])) middleList matchingGenreAnimes
            -- add the middleResult to the total accumulator list
            return (acc ++ middleResult)) [] fieldList

        filterM (\x -> return $ x /= inputAnime) result

{-
 - Input: A string of a genre stored in the database
 - Returns: A list of strings containing all shows that match the given genre
 -}
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

{-
 - Returns a list of pairs of strings, where each pair contains (title, url). The title is the name of show and the url is the link to the image related to the show
 -}
getTitles :: IO [(String, String)]
getTitles = do
    runSqlite "database.sqlite3" $ do
        titleList <- selectList [] []
        result <- foldM (\acc titleEntity -> do
            let title = entityVal titleEntity
            return (acc ++ [(titleName title, titleUrl title)])) [] titleList
        return result
        
{-
 - Returns a list of all genres in the database (for use in the UI)
 -}
getAllGenres :: IO [String]
getAllGenres = do
    runSqlite "database.sqlite3" $ do
        genresList <- selectList [] []
        result <- foldM (\acc genresEntity -> do
            let genre = entityVal genresEntity
            return (acc ++ [genresName genre])) [] genresList
        return $ nub result

