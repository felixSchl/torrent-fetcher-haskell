module Util (getCacheContents, getCachedMovieList, writeToCache)
where

import Config
import Models(Movie, MovieList)
import System.IO.Error(isDoesNotExistError)
import Control.Exception (try)
import Data.String.Utils(replace)
import Data.Aeson(decode)
import qualified Data.ByteString.Lazy.Char8 as BS

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
