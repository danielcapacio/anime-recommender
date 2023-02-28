module Main where
import           Database
import           Control.Concurrent.Async
import           Control.Monad.IO.Class (liftIO)
import           Data.Map as Map
import           Data.Array as Arr
import           Data.List as L
import           Control.Monad (forM_, join, foldM)
import           Data.Array.IO
import           Data.IORef
import           Data.Ord (comparing)
import           Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
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
    -- a <- async $ loadData -- we don't need to run this every time if we already have the database
    -- wait a
    -- start a server on port 8023 using the `startGUI` function
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "../wwwroot"
        } setup

joinWithComma :: [String] -> String
joinWithComma = L.foldr (\ s acc -> s ++ ", " ++ acc) ""

getRecommendation :: [String] -> String -> IO [String]  
getRecommendation genresList favouriteShow = do
    allGenresShow <- foldM (\acc x -> do
        showList <- getGenreShows x 
        return (showList ++ acc)) [] genresList

    commonThemes <- getThemes favouriteShow
    commonGenres <- getGenres favouriteShow
    commonStudios <- getStudios favouriteShow
    commonDemographics <- getDemographics favouriteShow

    let recommendation = allGenresShow ++ commonThemes ++ commonGenres ++ commonStudios ++ commonDemographics
    let sortedRecommendation = sortBy (flip $ comparing length) . group . sort $ recommendation -- returns 3 most common occurences in list
    return $ fst $ L.splitAt 3 $ L.map head sortedRecommendation

{-
 - Setup function that executes whenever a browser connects to the server.
 - Builds the initial HTML page with buttons and posters for the user to interact.
 -}
setup :: Window -> UI ()
setup window = do
    -- create an array ref to hold the user's TOP 5 anime genres
    topFiveGenreNamesRef <- liftIO $ newIORef []
    -- create an empty string to hold the user's selected favourite anime TITLE
    favouriteAnimeTitleRef <- liftIO $ newIORef ""
    
    return window # set UI.title "Anime Recommender"
    -- apply style to app body
    getBody window # set UI.style [
        ("text-align", "center"),
        ("margin-left", "100px"),
        ("margin-right", "100px"),
        ("font-family","Helvetica Neue"),
        ("background-color", "#7393B3"),
        ("color", "white")]
    applicationHeading <- UI.h1 # set text "Anime Recommmender Application"
    
    askTopGenresQuestion <- UI.h3 # set text "Please select your top 5 genres (in order from highest preference to lowest):"
    topFiveGenresOutput <- UI.div # set text "" # set UI.style [("margin", "10px")]
    labelTitle <- UI.div # set text "Your genre preferences:" # set UI.style [("margin", "10px"), ("display", "none")]
    -- create div to display user's selected genres
    genreDiv <- UI.div # set text "" # set UI.style [("margin", "10px")]

    -- add genre headers to app body
    getBody window #+ [element applicationHeading, element askTopGenresQuestion]
    
    -- create buttons for each genre
    genres <- liftIO $ getAllGenres
    genreButtons <- foldM(\acc genre -> do
        genreButton <- UI.button # set UI.text genre # set UI.style [("margin", "5px")]
        return (acc ++ [(genreButton, genre)])) [] genres
    genreButtonsDiv <- UI.div # set UI.style [("margin-left", "15%"), ("margin-right", "15%")]
    element genreButtonsDiv #+ L.map (\x -> element $ fst x) genreButtons

    -- add genre buttons and div to display their selections onto the app body
    getBody window #+ [element genreButtonsDiv, element labelTitle, element genreDiv]

    favouriteAnimeQuestion <- UI.h3 # set text "Now please select your favourite anime:"
    favouriteAnimeLabelTitle <- UI.div # set text "Your favourite (preferred) anime:" # set UI.style [("margin", "10px"), ("display", "none")]
    favouriteAnimeTitleCurrent <- UI.div # set text "" # set UI.style [("margin", "10px")]
    favouriteAnimeTitleOutput <- UI.div # set text "" # set UI.style [("margin", "10px")]
    -- hide list of anime posters until user picks their 5 genres
    animePostersDiv <- UI.div # set UI.style [("display", "none")]
    
    -- create button for sending user's preferences
    recommendationButton <- UI.button # set UI.text "Get Recommendation" # set UI.style [("display", "none")]
    -- create event handlers for each top anime calling function `initPosterAnchorHandlers`
    titles <- liftIO $ getTitles
    posters <- foldM(\acc (title,url) -> do
        poster <- initPosterAnchorHandlers title url favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton
        return (acc ++ [poster])) [] titles

    element animePostersDiv #+ ([element favouriteAnimeQuestion] ++ L.map element posters)
    getBody window #+ [element favouriteAnimeLabelTitle, element animePostersDiv, element favouriteAnimeLabelTitle, element favouriteAnimeTitleCurrent]

    -- styling the div to display our recommended titles
    recommendedTitleDiv <- UI.div # set UI.style [
            ("margin-bottom", "25px"), ("margin-left", "20%"), ("margin-right", "20%"),
            ("border", "2px solid white"),
            ("display", "none")]
    recommendedTitleHeader <- UI.h3 # set text "Your recommended title(s):"
    recommendedTitle <- UI.h2 # set text ""
    
    element recommendedTitleDiv #+ [element recommendedTitleHeader, element recommendedTitle]
    -- event handler for getting user's recommendation
    UI.on UI.click recommendationButton $ \_ -> do
        -- get the current genres array
        genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
        favouriteAnimePickedIO <- liftIO $ readIORef favouriteAnimeTitleRef
        element topFiveGenresOutput # set UI.text (show genresClickedArrIO)
        element favouriteAnimeTitleOutput # set UI.text favouriteAnimePickedIO
        liftIO $ print genresClickedArrIO -- *** print top 5 genres to console ***
        liftIO $ print favouriteAnimePickedIO -- *** print fav anime to console ***
        recommendation <- liftIO $ getRecommendation genresClickedArrIO favouriteAnimePickedIO
        liftIO $ print recommendation -- *** print recommendations to console ***
        element recommendedTitleDiv # set UI.style [("display", "")]
        element recommendedTitle # set UI.text (joinWithComma recommendation)
    
    -- setup elements to display recommended anime tile
    getRecommendationDiv <- UI.div # set UI.style [("padding", "10px")]
    element getRecommendationDiv #+ [element recommendationButton]
    element getRecommendationDiv #+ [element topFiveGenresOutput]
    element getRecommendationDiv #+ [element favouriteAnimeTitleOutput]
    getBody window #+ [element getRecommendationDiv]
    getBody window #+ [element recommendedTitleDiv]
    
    -- create event handlers for each genre button calling function `initGenreButtonHandlers`
    initGenreButtonHandlers genreButtons topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv

{-
 - Function to set up a click event handler for a genre button.
 - PARAMS:
 -      genreButtons            : button elements to be disabled upon click and added to user's preferences
 -      topFiveGenreNamesRef    : top 5 anime genres to update onClick of button
 -      labelTitle              : div with question prompt to ask user for top 5
 -      genreDiv                : div to update and display user's top 5 genres
 -      genreButtonsDiv         : div containing genre buttons to be displayed or hidden, controlled by a max of 5 genre selections
 -      animePostersDiv         : div containing all anime posters to be displayed when user has selected 5 genres
 - RETURNS:
 -      this function simply sets the event handlers for each button and updates appropriate divs to be hidden or displayed
 -}
initGenreButtonHandlers :: [(UI.Element, String)] -> IORef [String] -> UI.Element -> UI.Element -> UI.Element -> UI.Element -> UI ()
initGenreButtonHandlers genreButtons topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv = do
    forM_ genreButtons $ \(genreButton, genreName) -> do
        UI.on UI.click genreButton $ \_ -> do
            -- disabled clicked button to prevent duplicate genres from being picked
            element genreButton # set (UI.attr "disabled") ""
            -- display label title
            element labelTitle # set UI.style [("display", "")]
            -- get the current genres array
            genresClickedArrIO <- liftIO $ readIORef topFiveGenreNamesRef
            
            -- VALIDATION on getting user to pick 5 genres before picking an anime title
            if length genresClickedArrIO >= 4 then element animePostersDiv # set UI.style [("display", "")] else element genreButtonsDiv # set UI.style [("display", "none")]
            -- **HIDE** all genre buttons, else let user keep selecting their top 5 genres
            if length genresClickedArrIO >= 4 then element genreButtonsDiv # set UI.style [("display", "none")] else element genreButtonsDiv # set UI.style [("display", "")]
            
            -- add the name of the genre to the array
            let genresClickedArr = genresClickedArrIO ++ [genreName]
            -- update array
            liftIO $ writeIORef topFiveGenreNamesRef genresClickedArr
            -- update display div
            let genreDivText = joinWithComma genresClickedArr
            element genreDiv # set text genreDivText

{-
 - Function to set up a click event handler for an anime poster.
 - PARAMS:
 -      title                       : title of the anime
 -      image                       : image url
 -      favouriteAnimeTitleRef      : fave anime title string to update onClick of poster
 -      favouriteAnimeLabelTitle    : div displaying the clicked poster on recommendation click
 -      favouriteAnimeTitleCurrent  : div displaying the *currently* clicked poster (user might not have clicked `Get Recommendation` yet)
 -      recommendationButton        : the recommendation button to display once a user has successfully picked a fave anime
 - RETURNS:
 -      UI Element of an anchor tag containing an image tag with `src` attribute containing the url of the anime poster
 -}
initPosterAnchorHandlers :: String -> String -> IORef String -> UI.Element -> UI.Element -> UI.Element -> UI UI.Element
initPosterAnchorHandlers title image favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton = do
    posterLink <- UI.a
        # set (UI.attr "value") title
        # set UI.style [("margin", "10px")]
        #+ [UI.img # set UI.src image
            # set (UI.attr "width") "15%"]
    UI.on UI.click posterLink $ \_ -> do
        element recommendationButton # set UI.style [("display", "")]
        element favouriteAnimeLabelTitle # set UI.style [("display", "")]
        element favouriteAnimeTitleCurrent # set UI.text title
        -- update fav anime title string
        liftIO $ writeIORef favouriteAnimeTitleRef title
    return posterLink
