{-# LANGUAGE OverloadedStrings #-}

import System.IO.Error(isDoesNotExistError)
import Control.Exception (try)
import System.Environment (getArgs)
import Data.Map (Map, fromList)
import Data.String.Utils(replace)
import Network.HTTP(simpleHTTP, getRequest, getResponseBody)
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import qualified Text.Printf as Printf
import qualified Data.ByteString.Lazy.Char8 as BS

createCacheFile :: FilePath -> IO(Bool)
createCacheFile path = return(False)

type Action = [String] -> IO()
type Arguments = [String]

data MovieList = MovieList { getMovies :: [Movie]
                           }
                           deriving (Show)
instance FromJSON MovieList where
    parseJSON (Object v) =
        MovieList <$>
            (v .: "MovieList")
data Movie = Movie { getMovieID :: String
                   , getMovieTitle :: String
                   }
                   deriving (Show)
instance FromJSON Movie where
    parseJSON (Object v) =
        Movie <$>
            (v .: "MovieID")    <*>
            (v .: "MovieTitle")

-- Constants
cacheFilename = ".cache"
server = "http://yts.re/api/"

-- Returns the currently cached list
getCacheContents :: IO String
getCacheContents = do
    result <- try (readFile cacheFilename) :: IO (Either IOError String)
    case result of
        Left ex -> do
            case ex of
                isDoesNotExistError -> do
                    return ("")
        Right val -> do
            return (val)

-- Returns the in-memory version of the list
getCachedMovieList :: IO (Maybe MovieList)
getCachedMovieList = do
    contents <- getCacheContents
    let json = BS.pack $ replace "\\" "" contents
    return (decode json)

-- Write the last list to file
writeToCache :: String -> IO (Bool)
writeToCache s = do
    result <- try (writeFile cacheFilename s) :: IO (Either IOError ())
    case result of
        Left ex -> do
            return (False)
        Right val -> do
            return (True)

-- Debug wrapper
wrapAction :: String -> Action -> Arguments -> IO ()
wrapAction name action args = do
    putStrLn $ "Executing " ++ name ++ "..."
    action args

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

-- Download from last list
download :: Action
download args = do
    return()

-- Create a new list from a search
search :: Action
search args = do
    response <- simpleHTTP (getRequest (server ++ "list.json"))
    json <- getResponseBody response
    writeToCache json
    return()

-- Routes the input arguments to actions
executeRoute :: [String] -> IO ()
executeRoute ("show":     args) = wrapAction "show" showlast args
executeRoute ("download": args) = wrapAction "download" download args
executeRoute ("search":   args) = wrapAction "search" search args
executeRoute []                 = error "No arguments provided"

main :: IO()
main = do
    arguments <- getArgs
    executeRoute arguments
    return()
