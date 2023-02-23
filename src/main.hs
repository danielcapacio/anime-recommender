module Main where
import           InitializeDatabase
import           Control.Concurrent.Async
import           Control.Monad.IO.Class (liftIO)

main = do
    a <- async $ loadData
    wait a

    result <- getGenre "Fullmetal Alchemist: Brotherhood"
    liftIO $ print result

