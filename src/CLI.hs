module CLI where
import Data.Map as Map
import Data.Aeson.Encoding (list)


printListOfGenres :: IO ()
printListOfGenres = 
    putStrLn "1 - Action\n2 - Adventure\n3 - Award Winning\n4 - Comedy\n5 - Drama\n6 - Ecchi\n7 - Fantasy\n8 - Mystery\n9 - Romance\n10 - Sci-Fi\n11 - Slice of Life\n12 - Sports\n13 - Supernatural\n14 - Suspense\n"

{-
 - List of genres for user to rank their preferences
 -}
listOfGenres :: Map.Map String String
listOfGenres = Map.fromList [
    ("1", "Action"),
    ("2", "Adventure"),
    ("3", "Award Winning"),
    ("4", "Comedy"),
    ("5", "Drama"),
    ("6", "Ecchi"),
    ("7", "Fantasy"),
    ("8", "Mystery"),
    ("9", "Romance"),
    ("10", "Sci-Fi"),
    ("11", "Slice of Life"),
    ("12", "Sports"),
    ("13", "Supernatural"),
    ("14", "Suspense")]

getListOfGenres :: Map String String
getListOfGenres = listOfGenres

deleteGenre :: String -> Map.Map String String
deleteGenre genre = Map.delete genre listOfGenres

{-
 - Initialized map with key values (user-ranking) pairing to individual string values (anime genre)
 -}
userGenreRankings :: Map.Map Int String
userGenreRankings = empty

-- lookup a value in the genre map
lookupResult :: Maybe String
lookupResult = Map.lookup 0 userGenreRankings

-- insert a new key-value pair into the genre rankings map
newMap :: Map.Map Int String
newMap = Map.insert 1 "One Piece" userGenreRankings
