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

joinWithComma :: [String] -> String
joinWithComma = L.foldr (\ s acc -> s ++ ", " ++ acc) ""

setup :: Window -> UI ()
setup window = do
    -- create an array ref to hold the user's TOP 5 anime genres
    topFiveGenreNamesRef <- liftIO $ newIORef []
    -- create an empty string to hold user's single, selected anime TITLE
    favouriteAnimeTitleRef <- liftIO $ newIORef ""
    
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

    favouriteAnimeQuestion <- UI.h3 # set text "Please select your favourite anime:"
    getBody window #+ [element favouriteAnimeQuestion]
    -- create the anchor tag with the image and event handler
    aImg <- UI.a # set UI.href "#" 
                # set (UI.attr "value") "putAnimeTitleHere"
                #+ [UI.img # set UI.src "https://cdn.myanimelist.net/images/anime/7/3791.jpg"
                    # set (UI.attr "width") "25%"]
    UI.on UI.click aImg $ \_ -> do
        -- get the value attribute of the clicked anchor tag
        animeTitle <- UI.get UI.value aImg
        -- update array
        liftIO $ writeIORef favouriteAnimeTitleRef animeTitle
        favouriteAnimePicked <- liftIO $ readIORef favouriteAnimeTitleRef
        -- liftIO $ print favouriteAnimePicked -- print picked genres to console
        liftIO $ print favouriteAnimePicked
    -- anchor tag to the body element
    getBody window #+ [element aImg]

    -- button for sending user's preferences
    getRecommendationButton <- UI.button # set UI.text "Get Recommendation" # set UI.style [("display", "none")]
    -- getBody window #+ [element getRecommendationButton]
    -- event handler for getting user's recommendation
    UI.on UI.click getRecommendationButton $ \_ -> do
        -- get the current genres array
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        element topFiveGenresOutput # set UI.text (show genresClickedArrIO)
        liftIO $ print genresClickedArrIO -- print picked genres to console
    
    getRecommendationDiv <- UI.div # set UI.style [("padding", "10px")]
    element getRecommendationDiv #+ [element getRecommendationButton]
    element getRecommendationDiv #+ [element topFiveGenresOutput]
    getBody window #+ [element getRecommendationDiv]
    
    -- event handlers for each genre button
    UI.on UI.click genreButton1 $ \_ -> do
        -- disabled clicked button to prevent duplicate genres from being picked
        element genreButton1 # set (UI.attr "disabled") ""
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
        element genreButton2 # set (UI.attr "disabled") ""
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Adventure"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton3 $ \_ -> do
        element genreButton3 # set (UI.attr "disabled") ""
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Award Winning"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton4 $ \_ -> do
        element genreButton4 # set (UI.attr "disabled") ""
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Comedy"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton5 $ \_ -> do
        element genreButton5 # set (UI.attr "disabled") ""
        element labelTitle # set UI.style [("display", "Drama")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Drama"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton6 $ \_ -> do
        element genreButton6 # set (UI.attr "disabled") ""
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Ecchi"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton7 $ \_ -> do
        element genreButton7 # set (UI.attr "disabled") ""
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Fantasy"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton8 $ \_ -> do
        element genreButton8 # set (UI.attr "disabled") ""
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Mystery"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton9 $ \_ -> do
        element genreButton9 # set (UI.attr "disabled") ""
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Romance"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton10 $ \_ -> do
        element genreButton10 # set (UI.attr "disabled") ""
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Sci-Fi"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton11 $ \_ -> do
        element genreButton11 # set (UI.attr "disabled") ""
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Slice of Life"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton12 $ \_ -> do
        element genreButton12 # set (UI.attr "disabled") ""
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Sports"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton13 $ \_ -> do
        element genreButton13 # set (UI.attr "disabled") ""
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Supernatural"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText

    UI.on UI.click genreButton14 $ \_ -> do
        element genreButton14 # set (UI.attr "disabled") ""
        element labelTitle # set UI.style [("display", "")]
        element getRecommendationButton # set UI.style [("display", "")]
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        let genresClickedArr = genresClickedArrIO ++ ["Suspense"]
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText
