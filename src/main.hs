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

    favouriteAnimeQuestion <- UI.h3 # set text "Now please select your favourite anime:"
    favouriteAnimeLabelTitle <- UI.div # set text "Your favourite (preferred) anime:" # set UI.style [("margin", "10px"), ("display", "none")]
    favouriteAnimeTitleCurrent <- UI.div # set text "" # set UI.style [("margin", "10px")]
    favouriteAnimeTitleOutput <- UI.div # set text "" # set UI.style [("margin", "10px")]
    animePostersDiv <- UI.div # set UI.style [("display", "none")]
    
    -- button for sending user's preferences
    recommendationButton <- UI.button # set UI.text "Get Recommendation" # set UI.style [("display", "none")]
    -- TODO: need to be able to handle an arbitrary amount
    -- create event handlers for each top anime calling function `initPosterAnchorHandlers`
    animePoster1 <- initPosterAnchorHandlers "Violet Evergarden Movie" "https://cdn.myanimelist.net/images/anime/1032/100778.jpg" favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton
    animePoster2 <- initPosterAnchorHandlers "Cowboy Bepop" "https://cdn.myanimelist.net/images/anime/7/3791.jpg" favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton
    animePoster3 <- initPosterAnchorHandlers "Fullmetal Alchemist: Brotherhood" "https://cdn.myanimelist.net/images/anime/2/17090.jpg" favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton
    animePoster4 <- initPosterAnchorHandlers "Fruits Basket: The Final" "https://cdn.myanimelist.net/images/anime/1085/114792.jpg" favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton
    animePoster5 <- initPosterAnchorHandlers "Code Geass: Hangyaku no Lelouch R2" "https://cdn.myanimelist.net/images/anime/1736/109854.jpg" favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton
    animePoster6 <- initPosterAnchorHandlers "Hunter x Hunter (2011)" "https://cdn.myanimelist.net/images/anime/1639/92662.jpg" favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton
    animePoster7 <- initPosterAnchorHandlers "Steins;Gate" "https://cdn.myanimelist.net/images/anime/10/32023.jpg" favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton
    animePoster8 <- initPosterAnchorHandlers "Bleach: Sennen Kessen-hen" "https://cdn.myanimelist.net/images/anime/1731/124971.jpg" favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton
    animePoster9 <- initPosterAnchorHandlers "Bocchi the Rock!" "https://cdn.myanimelist.net/images/anime/1705/120728.jpg" favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton
    animePoster10 <- initPosterAnchorHandlers "Owarimonogatari 2nd Season" "https://cdn.myanimelist.net/images/anime/2/89401.jpg" favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton
    animePoster11 <- initPosterAnchorHandlers "Koe no Katachi" "https://cdn.myanimelist.net/images/anime/3/80136.jpg" favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton
    animePoster12 <- initPosterAnchorHandlers "Gintama" "https://cdn.myanimelist.net/images/anime/13/83412.jpg" favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton
    animePoster13 <- initPosterAnchorHandlers "Monster" "https://cdn.myanimelist.net/images/anime/10/13733.jpg" favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton
    animePoster14 <- initPosterAnchorHandlers "Ginga Eiyuu Densetsu" "https://cdn.myanimelist.net/images/anime/2/74210.jpg" favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton
    animePoster15 <- initPosterAnchorHandlers "3-gatsu no Lion 2nd Season" "https://cdn.myanimelist.net/images/anime/1429/100628.jpg" favouriteAnimeTitleRef favouriteAnimeLabelTitle favouriteAnimeTitleCurrent recommendationButton
    -- add anime posters to div and to body
    element animePostersDiv #+ [
            element favouriteAnimeQuestion,
            element animePoster1, element animePoster2, element animePoster3,
            element animePoster4, element animePoster5, element animePoster6,
            element animePoster7, element animePoster8, element animePoster9,
            element animePoster10, element animePoster11, element animePoster12,
            element animePoster13, element animePoster14, element animePoster15
        ]
    getBody window #+ [element favouriteAnimeLabelTitle, element animePostersDiv, element favouriteAnimeLabelTitle, element favouriteAnimeTitleCurrent]

    recommendedTitleDiv <- UI.div # set UI.style [
            ("margin-bottom", "25px"), ("margin-left", "20%"), ("margin-right", "20%"),
            ("border", "2px solid black"),
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
        -- TODO: call our filter algorithm based on `topFiveGenreNamesRef` and `favouriteAnimeTitleRef`
        element recommendedTitleDiv # set UI.style [("display", "")]
        element recommendedTitle # set UI.text "<PUT GENERATED TITLE(S) HERE>"
        
    -- setup elements to display recommended anime tile
    getRecommendationDiv <- UI.div # set UI.style [("padding", "10px")]
    element getRecommendationDiv #+ [element recommendationButton]
    element getRecommendationDiv #+ [element topFiveGenresOutput]
    element getRecommendationDiv #+ [element favouriteAnimeTitleOutput]
    getBody window #+ [element getRecommendationDiv]
    getBody window #+ [element recommendedTitleDiv]
    
    -- create event handlers for each genre button calling function `initGenreButtonHandlers`
    initGenreButtonHandlers genreButton1 "Action" topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv
    initGenreButtonHandlers genreButton2 "Adventure" topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv
    initGenreButtonHandlers genreButton3 "Award Winning" topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv
    initGenreButtonHandlers genreButton4 "Comedy" topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv
    initGenreButtonHandlers genreButton5 "Drama" topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv
    initGenreButtonHandlers genreButton6 "Ecchi" topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv
    initGenreButtonHandlers genreButton7 "Fantasy" topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv
    initGenreButtonHandlers genreButton8 "Mystery" topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv
    initGenreButtonHandlers genreButton9 "Romance" topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv
    initGenreButtonHandlers genreButton10 "Sci-Fi" topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv
    initGenreButtonHandlers genreButton11 "Slice of Life" topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv
    initGenreButtonHandlers genreButton12 "Sports" topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv
    initGenreButtonHandlers genreButton13 "Supernatural" topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv
    initGenreButtonHandlers genreButton14 "Suspense" topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv

{-
 - function to set up a click event handler for a genre button 
 -}
initGenreButtonHandlers :: UI.Element -> String -> IORef [String] -> UI.Element -> UI.Element -> UI.Element -> UI.Element -> UI ()
initGenreButtonHandlers genreButton genreName topFiveGenreNamesRef labelTitle genreDiv genreButtonsDiv animePostersDiv = do
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
 - function to set up a click event handler for an anime poster
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
