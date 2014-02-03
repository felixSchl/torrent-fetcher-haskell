module Cli(printMovieTable)
where

import Models(Movie(..), MovieList(..))
import qualified Text.Printf as Printf

data ListFormat = ListFormat { getIdWidth :: Int
                             , getTitleWidth :: Int
                             }
-- Print movie list
printMovieTable :: MovieList -> IO ()
printMovieTable ml = do
    let movies = getMovies ml
    let format = getMovieListFormat movies
    printMovieRows movies format

printMovieRows :: [Movie] -> ListFormat -> IO ()
printMovieRows (m:xs) format = do
    let id = getMovieID m
    let title = getMovieTitle m

    -- pad the columns
    let col1 = Printf.printf ("%-"++(show $ getIdWidth format) ++"s | ") id
    let col2 = Printf.printf ("%-"++(show $ getTitleWidth format) ++"s | ") title
    let s = col1 ++ col2
    putStrLn s

    printMovieRows xs format
printMovieRows _ _ = return ()

getMovieListFormat :: [Movie] -> ListFormat
getMovieListFormat movies@(m:xs) = do
    let ids = [length $ getMovieID x | x <- movies]
    let titles = [length $ getMovieTitle x | x <- movies]
    ListFormat { getIdWidth = foldl max 0 ids
               , getTitleWidth = foldl max 0 titles
               }
