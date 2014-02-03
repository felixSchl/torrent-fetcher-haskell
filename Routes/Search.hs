module Routes.Search (search)
where

import Config
import Types
import Network.HTTP(simpleHTTP, getRequest, getResponseBody)
import Config(cacheFilename, server)
import System.Exit(exitWith, ExitCode(..))
import System.IO(stderr, hPutStrLn)
import System.Environment(getProgName)
import System.Console.GetOpt(usageInfo, ArgOrder(..), getOpt, OptDescr(..), ArgDescr(..))
import Util(writeToCache)

type Keywords = [String]
type Rating = Integer
data Ordering = Date | Seeds | Peers | Size | Alphabet | Rating | Download |
                Year
data Soring = Asc | Desc
data Genre = All
data Quality = Q720P | Q1080P | Q3D

data Options = Options { optLimit :: Integer
                       , optSet :: Integer
                       }

{- Arguments:
 - limit :: Integer (1-50)
 - set :: Integer
 - quality :: Quality
 - rating :: Rating
 - keywords :: [String]
 - genre :: Genre
 - sort :: Sorting
 - Order :: Ordering
 - -}

startOptions :: Options
startOptions = Options { optLimit = 50
                       , optSet = 1
                       }

options :: [OptDescr (Options -> IO Options)]
options = 
    [ Option "l" ["limit"]
        (ReqArg 
            (\arg opt -> do 
                let n = read arg :: Integer
                return opt { optLimit = maximum([0, minimum([50, n])]) })
            "Int between 1 - 50 (inclusive)")
        "Determines the max amount of movie results"
    , Option "s" ["set"]
        (ReqArg 
            (\arg opt -> do
                let n = read arg :: Integer
                return opt { optSet = n })
            "Int")
        "Used to see the next set of movies, eg limit=15 and set=2 will show you movies 15-30"
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
            prg <- getProgName
            hPutStrLn stderr (usageInfo prg options)
            exitWith ExitSuccess
            ))
        "Show help"
    ]


-- Create a new list from a search
search :: Action
search args = do
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    let Options { optLimit = limit, optSet = set } = opts

    putStrLn ("Limit: " ++ (show limit))
    putStrLn ("Set: " ++ (show set))

    -- response <- simpleHTTP (getRequest (server ++ "list.json"))
    -- json <- getResponseBody response
    -- writeToCache json

    return()

