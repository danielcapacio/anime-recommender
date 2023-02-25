module Main where
import           InitializeDatabase
import           CLI
import           Control.Concurrent.Async
import           Control.Monad.IO.Class (liftIO)
import           Data.Map as Map
import           Data.Array as Arr
import           Data.List as L
import           Control.Monad (forM_, join)
import           Data.Array.IO
import           Data.IORef
import qualified Graphics.UI.Threepenny       as UI
import Graphics.UI.Threepenny.Core
    ( defaultConfig,
      Config(jsStatic, jsPort),
      (#),
      (#+),
      children,
      column,
      element,
      getBody,
      set,
      sink,
      text,
      startGUI,
      accumB,
      UI,
      Window )

main = do
    -- start a server on port 8023 using the `startGUI` function
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "../wwwroot"
        } setup
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

joinWithComma :: [String] -> String
joinWithComma = L.foldr (\ s acc -> s ++ " " ++ acc) ""

setup :: Window -> UI ()
setup window = do
    -- create an array ref to hold the user's TOP 5 anime genres
    topFiveGenreNamesRef <- liftIO $ newIORef []
    
    return window # set UI.title "Anime Recommender"
    -- style <body></body>
    getBody window # set UI.style [("text-align", "center"), ("margin-left", "100px"), ("margin-right", "100px")]
    applicationHeading <- UI.h1 # set text "Anime Recommmender Application"
    
    -- div to display the concatenated button names
    askTopGenresQuestion <- UI.h3 # set text "Please select your top 5 genres (in order from highest preference to lowest):"
    topFiveGenresOutput <- UI.div # set text "" # set UI.style [("margin", "10px")]
    labelTitle <- UI.div # set text "Your genre preferences:" # set UI.style [("margin", "10px"), ("display", "none")]
    -- div to display to user
    genreDiv <- UI.div # set text "" # set UI.style [("margin", "10px")]

    -- add headers to app body
    getBody window #+ [element applicationHeading]
    getBody window #+ [element askTopGenresQuestion]
    
    -- buttons for each genre
    genreButton1 <- UI.button # set UI.text "Action" # set UI.style [("margin", "5px")]
    genreButton2 <- UI.button # set UI.text "Adventure" # set UI.style [("margin", "5px")]
    genreButton3 <- UI.button # set UI.text "Award Winning" # set UI.style [("margin", "5px")]
    genreButton4 <- UI.button # set UI.text "Comedy" # set UI.style [("margin", "5px")]
    genreButton5 <- UI.button # set UI.text "Drama" # set UI.style [("margin", "5px")]
    genreButton6 <- UI.button # set UI.text "Ecchi" # set UI.style [("margin", "5px")]
    genreButton7 <- UI.button # set UI.text "Fantasy" # set UI.style [("margin", "5px")]
    genreButton8 <- UI.button # set UI.text "Mystery" # set UI.style [("margin", "5px")]
    genreButton9 <- UI.button # set UI.text "Romance" # set UI.style [("margin", "5px")]
    genreButton10 <- UI.button # set UI.text "Sci-Fi" # set UI.style [("margin", "5px")]
    genreButton11 <- UI.button # set UI.text "Slice of Life" # set UI.style [("margin", "5px")]
    genreButton12 <- UI.button # set UI.text "Sports" # set UI.style [("margin", "5px")]
    genreButton13 <- UI.button # set UI.text "Supernatural" # set UI.style [("margin", "5px")]
    genreButton14 <- UI.button # set UI.text "Suspense" # set UI.style [("margin", "5px")]
    
    -- add buttons to app body
    getBody window #+ [element genreButton1, element genreButton2, element genreButton3, 
        element genreButton4, element genreButton5, element genreButton6, element genreButton7,
        element genreButton8, element genreButton9, element genreButton10, element genreButton11,
        element genreButton12, element genreButton13, element genreButton14]
    
    getBody window #+ [element labelTitle, element genreDiv]

    -- add a button for sending user's preferences
    getRecommendationButton <- UI.button # set UI.text "Get Recommendation" # set UI.style [("display", "none")]
    getBody window #+ [element getRecommendationButton]
    UI.on UI.click getRecommendationButton $ \_ -> do
        -- get the current genres array
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        element topFiveGenresOutput # set UI.text (show genresClickedArrIO)
        liftIO $ print genresClickedArrIO -- print picked genres to console
    getBody window #+ [element topFiveGenresOutput]
    
    -- event handlers for each genre button
    UI.on UI.click genreButton1 $ \_ -> do
        -- display label title
        element labelTitle # set UI.style [("display", "")]
        -- TODO: validation for 5 genres to be selected
        element getRecommendationButton # set UI.style [("display", "")]
        -- get the current genres array
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        
        -- TODO: do not know the best way to disable all buttons after 5 genres selected
        -- disable all genre buttons, else let user keep selecting their top 5 genres
        -- if length genresClickedArrIO >= 4 then (element genreButton1 # set (UI.attr "disabled") "") else (element genreButton1 # set (UI.attr "class") "")
        
        -- add the name of the button to the array
        let genresClickedArr = genresClickedArrIO ++ ["Action"]
        -- update array
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        -- update display div
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton2 $ \_ -> do
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Adventure"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton3 $ \_ -> do
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Award Winning"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton4 $ \_ -> do
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Comedy"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton5 $ \_ -> do
        element labelTitle # set UI.style [("display", "Drama")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Drama"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton6 $ \_ -> do
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Ecchi"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton7 $ \_ -> do
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Fantasy"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton8 $ \_ -> do
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Mystery"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton9 $ \_ -> do
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Romance"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton10 $ \_ -> do
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Sci-Fi"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton11 $ \_ -> do
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Slice of Life"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton12 $ \_ -> do
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Sports"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton13 $ \_ -> do
        element labelTitle # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Supernatural"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton14 $ \_ -> do
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Suspense"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText
    
    -- TODO: working
    -- return window # set UI.title "Anime Recommender"
    -- -- Create an array of button labels
    -- let buttonLabels = ["Button 1", "Button 2", "Button 3", "Button 4", "Button 5"]

    -- -- Create a list of buttons from the array
    -- buttons <- sequence $ [UI.button # set text label | label <- buttonLabels]

    -- -- Add the buttons to the UI
    -- let buttonElements = [element button | button <- buttons]
    -- buttonsDiv <- UI.grid $ pure buttonElements
    -- comedy <- UI.button # set UI.text "Comedy"
    -- getBody window #+ [element comedy]
    -- getBody window #+ [element buttonsDiv]
    -- UI.on UI.click comedy $ const $ do
    --     element comedy # set UI.text "I have been clicked!"
