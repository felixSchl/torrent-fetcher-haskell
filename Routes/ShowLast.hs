module Routes.ShowLast (showlast)
where

import Types
import Models(Movie(..), MovieList(..))
import Util(getCachedMovieList)
import Cli(printMovies, getMovieListFormat)

-- Show last list
showlast :: Action
showlast args = do
    result <- getCachedMovieList
    case result of
        Just ml -> do
            let movies = getMovies ml
            let format = getMovieListFormat movies
            printMovies movies format
        -- XXX: Propagate reason of failure, e.g. could not deserialize or
        -- no existing cache etc.:
        Nothing -> error "No movie list"
    return()
