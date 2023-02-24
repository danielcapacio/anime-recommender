module Main where
import           InitializeDatabase
import           CLI
import           Control.Concurrent.Async
import           Control.Monad.IO.Class (liftIO)
import           Data.Map as Map
import           Control.Monad (forM_)
import           Data.Array.IO

main = do
    -- a <- async $ loadData -- we don't need to run this every time if we already have the database 
    -- wait a
    
    -- result <- getGenre "Fullmetal Alchemist: Brotherhood"
    -- liftIO $ print result

    -- initialize a mutable IO array
    genreRankingsArr <- newArray (1,5) 99 :: IO (IOArray Int Int)
    
    putStrLn "Welcome to the Anime Recommender Application\n"
    -- ask for user's top 5 genres
    putStrLn "What are your top 5 genres? (from highest preference to lowest)"
    
    printListOfGenres
    putStrLn "Type in the number corresponding to the genre:"
    -- display list of all genres that user has not selected yet
    -- forM_ (Map.toList listOfGenres) $ \(k,v) ->
    --     putStrLn $ k ++ " - " ++ show v
    -- TODO: implement fixedGetLine
    -- account for bad user input: (i.e. - not a number, not a valid number)
    firstChoice <- getLine
    writeArray genreRankingsArr 1 (read firstChoice :: Int)

    putStrLn "Type in the number corresponding to the genre:"
    secondChoice <- getLine
    -- TODO: implement check so that user does not input same/repeat genres
    -- TODO: input validation: i.e. - string inputs, integers not within range of the given list
    writeArray genreRankingsArr 2 (read secondChoice :: Int)

    putStrLn "Type in the number corresponding to the genre:"
    thirdChoice <- getLine
    writeArray genreRankingsArr 3 (read thirdChoice :: Int)

    putStrLn "Type in the number corresponding to the genre:"
    fourthChoice <- getLine
    writeArray genreRankingsArr 4 (read fourthChoice :: Int)
    
    putStrLn "Type in the number corresponding to the genre:"
    fifthChoice <- getLine
    writeArray genreRankingsArr 5 (read fifthChoice :: Int)
    
    putStrLn "\nWhat is your favourite anime?"

    -- update userGenreRankings map; insert
    -- Map.insert "1" "one piece" userGenreRankings
    -- update listOfGenres; remove chosen genre
    putStrLn ""
