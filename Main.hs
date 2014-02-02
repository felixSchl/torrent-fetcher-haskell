import System.IO.Error(isDoesNotExistError)
import Control.Exception (try)
import System.Environment (getArgs)
import Data.Map (Map, fromList)
import Network.HTTP(simpleHTTP, getRequest, getResponseBody)

createCacheFile :: FilePath -> IO(Bool)
createCacheFile path = return(False)

type Action = [String] -> IO()
type Arguments = [String]

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

-- Show last list
showlast :: Action
showlast args = do
    contents <- getCacheContents
    putStrLn contents
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
