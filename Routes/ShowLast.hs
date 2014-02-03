module Routes.ShowLast (showlast)
where

import Types
import Models(Movie(..), MovieList(..))
import Util(getCachedMovieList)
import Cli(printMovieTable)

-- Show last list
showlast :: Action
showlast args = do
    result <- getCachedMovieList
    case result of
        Just ml -> do
            printMovieTable ml
        -- XXX: Propagate reason of failure, e.g. could not deserialize or
        -- no existing cache etc.:
        Nothing -> error "No movie list"
    return()
