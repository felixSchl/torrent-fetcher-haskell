module Routes.Download(download)
where

import Types
import Models(Movie(..), MovieList(..), getMovieById)
import System.IO(hFlush, hPutStrLn, stdout, stderr)
import Util(getCachedMovieList)
import Cli(printMovieTable)

askForMovie :: MovieList -> IO (Maybe Movie)
askForMovie ml = do
    putStr "Choose movie (ID): "
    hFlush stdout
    choice <- getLine
    let movie = getMovieById ml choice
    case movie of
        Just m -> do
            putStrLn $ "Selected: " ++ (getMovieTitle m)
            return (Just m)
        Nothing -> do
            hPutStrLn stderr "Invalid choice!"
            hFlush stderr
            askForMovie ml

-- Download from last list
download :: Action
download args = do
    result <- getCachedMovieList
    case result of
        Just ml -> do
            printMovieTable ml
            m <- askForMovie ml
            case m of
                Just m' -> do
                    return()
                _ -> error "No movie selected!"
            return ()
        Nothing -> error "No movie list"
    return()
