module Cli(printMovies, getMovieListFormat)
where

import Models(Movie(..), MovieList(..))
import qualified Text.Printf as Printf

data ListFormat = ListFormat { getIdWidth :: Int
                             , getTitleWidth :: Int
                             }
-- Print movie list
printMovies :: [Movie] -> ListFormat -> IO ()
printMovies (m:xs) format = do
    let id = getMovieID m
    let title = getMovieTitle m

    -- pad the columns
    let col1 = Printf.printf ("%-"++(show $ getIdWidth format) ++"s | ") id
    let col2 = Printf.printf ("%-"++(show $ getTitleWidth format) ++"s | ") title
    let s = col1 ++ col2
    putStrLn s

    printMovies xs format
printMovies _ _ = return ()

getMovieListFormat :: [Movie] -> ListFormat
getMovieListFormat movies@(m:xs) = do
    let ids = [length $ getMovieID x | x <- movies]
    let titles = [length $ getMovieTitle x | x <- movies]
    ListFormat { getIdWidth = foldl max 0 ids
               , getTitleWidth = foldl max 0 titles
               }
