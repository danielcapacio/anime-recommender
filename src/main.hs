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
    
    -- add buttons to a separate div
    genreButtonsDiv <- UI.div # set UI.style [("margin-left", "15%"), ("margin-right", "15%")]
    element genreButtonsDiv #+ [element genreButton1, element genreButton2, element genreButton3, 
        element genreButton4, element genreButton5, element genreButton6, element genreButton7,
        element genreButton8, element genreButton9, element genreButton10, element genreButton11,
        element genreButton12, element genreButton13, element genreButton14]
    
    getBody window #+ [element genreButtonsDiv]
    getBody window #+ [element labelTitle, element genreDiv]

    favouriteAnimeQuestion <- UI.h3 # set text "Please select your favourite anime:"
    getBody window #+ [element favouriteAnimeQuestion]
    -- create the anchor tag with the image and event handler
    aImg <- UI.a # set UI.href "#" 
                # set (UI.attr "value") "putAnimeTitleHere"
                #+ [UI.img # set UI.src "https://cdn.myanimelist.net/images/anime/7/3791.jpg"
                    # set (UI.attr "width") "15%"]
    UI.on UI.click aImg $ \_ -> do
        -- get the value attribute of the clicked anchor tag
        animeTitle <- UI.get UI.value aImg
        -- update array
        liftIO $ writeIORef favouriteAnimeTitleRef animeTitle
        favouriteAnimePicked <- liftIO $ readIORef favouriteAnimeTitleRef
        liftIO $ print favouriteAnimePicked -- *** print fav anime to console ***
    -- anchor tag to the body element
    getBody window #+ [element aImg]

    recommendedTitleDiv <- UI.div # set UI.style [
            ("margin-bottom", "25px"), ("margin-left", "20%"), ("margin-right", "20%"),
            ("border", "2px solid black"),
            ("display", "none")]
    recommendedTitleHeader <- UI.h3 # set text "Your recommended title(s):"
    recommendedTitle <- UI.h2 # set text ""
    
    -- button for sending user's preferences
    getRecommendationButton <- UI.button # set UI.text "Get Recommendation" # set UI.style [("display", "none")]
    element recommendedTitleDiv #+ [element recommendedTitleHeader, element recommendedTitle]
    -- event handler for getting user's recommendation
    UI.on UI.click getRecommendationButton $ \_ -> do
        -- get the current genres array
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        element topFiveGenresOutput # set UI.text (show genresClickedArrIO)
        liftIO $ print genresClickedArrIO -- *** print top 5 genres to console ***
        -- TODO: call our filter algorithm based on `topFiveGenreNamesRef` and `favouriteAnimeTitleRef`
        element recommendedTitleDiv # set UI.style [("display", "")]
        element recommendedTitle # set UI.text "<PUT GENERATED TITLE(S) HERE>"
        
    -- setup elements to display recommended anime tile
    getRecommendationDiv <- UI.div # set UI.style [("padding", "10px")]
    element getRecommendationDiv #+ [element getRecommendationButton]
    element getRecommendationDiv #+ [element topFiveGenresOutput]
    getBody window #+ [element getRecommendationDiv]
    getBody window #+ [element recommendedTitleDiv]
    
    -- create event handlers for each genre button calling function `setupGenreButton`
    setupGenreButton genreButton1 "Action" topFiveGenreNamesRef labelTitle getRecommendationButton genreDiv
    setupGenreButton genreButton2 "Adventure" topFiveGenreNamesRef labelTitle getRecommendationButton genreDiv
    setupGenreButton genreButton3 "Award Winning" topFiveGenreNamesRef labelTitle getRecommendationButton genreDiv
    setupGenreButton genreButton4 "Comedy" topFiveGenreNamesRef labelTitle getRecommendationButton genreDiv
    setupGenreButton genreButton5 "Drama" topFiveGenreNamesRef labelTitle getRecommendationButton genreDiv
    setupGenreButton genreButton6 "Ecchi" topFiveGenreNamesRef labelTitle getRecommendationButton genreDiv
    setupGenreButton genreButton7 "Fantasy" topFiveGenreNamesRef labelTitle getRecommendationButton genreDiv
    setupGenreButton genreButton8 "Mystery" topFiveGenreNamesRef labelTitle getRecommendationButton genreDiv
    setupGenreButton genreButton9 "Romance" topFiveGenreNamesRef labelTitle getRecommendationButton genreDiv
    setupGenreButton genreButton10 "Sci-Fi" topFiveGenreNamesRef labelTitle getRecommendationButton genreDiv
    setupGenreButton genreButton11 "Slice of Life" topFiveGenreNamesRef labelTitle getRecommendationButton genreDiv
    setupGenreButton genreButton12 "Sports" topFiveGenreNamesRef labelTitle getRecommendationButton genreDiv
    setupGenreButton genreButton13 "Supernatural" topFiveGenreNamesRef labelTitle getRecommendationButton genreDiv
    setupGenreButton genreButton14 "Suspense" topFiveGenreNamesRef labelTitle getRecommendationButton genreDiv

-- function to set up a click event handler for a genre button
setupGenreButton :: UI.Element -> String -> IORef [String] -> UI.Element -> UI.Element -> UI.Element -> UI ()
setupGenreButton genreButton genreName topFiveGenreNamesRef labelTitle getRecommendationButton genreDiv = do
    UI.on UI.click genreButton $ \_ -> do
        -- disabled clicked button to prevent duplicate genres from being picked
        element genreButton # set (UI.attr "disabled") ""
        -- display label title
        element labelTitle # set UI.style [("display", "")]
        -- TODO: validation for 5 genres to be selected
        element getRecommendationButton # set UI.style [("display", "")]
        -- get the current genres array
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        
        -- TODO: need to best way to disable all buttons after 5 genres selected
        -- disable all genre buttons, else let user keep selecting their top 5 genres
        -- if length genresClickedArrIO >= 4 then (element genreButton1 # set (UI.attr "disabled") "") else (element genreButton1 # set (UI.attr "class") "")
        
        -- add the name of the genre to the array
        let genresClickedArr = genresClickedArrIO ++ [genreName]
        -- update array
        liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
        -- update display div
        let genreDivText = joinWithComma genresClickedArr
        element genreDiv # set text genreDivText
