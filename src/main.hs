module Main where
import           InitializeDatabase
import           Control.Concurrent.Async
import           Control.Monad.IO.Class (liftIO)

main = do
    a <- async $ loadData -- we don't need to run this every time if we already have the database 
    wait a

    result <- getGenre "Fullmetal Alchemist: Brotherhood"
    liftIO $ print result

