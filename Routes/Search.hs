module Routes.Search (search)
where

import Config
import Types
import Util(writeToCache)
import Network.HTTP(simpleHTTP, getRequest, getResponseBody)
import Config(cacheFilename, server)
import System.Exit(exitWith, ExitCode(..))
import System.IO(stderr, hPutStrLn)
import System.Environment(getProgName)
import System.Console.GetOpt(usageInfo, ArgOrder(..), getOpt, OptDescr(..), ArgDescr(..))
import Data.String.Utils(join)
import Data.Char(toLower)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types(renderQuery, Query(..), QueryItem(..))

type Keywords = [String]
type Rating = Integer
data Sorting = Date | Seeds | Peers | Size | Alphabet |
               Rating | Download | Year
               deriving (Show, Enum)
data Order = Asc | Desc deriving (Show, Enum)
data Genre = AllGenres deriving (Enum)
instance Show Genre where
    show AllGenres = "ALL"
data Quality = QAll | Q720P | Q1080P | Q3D deriving (Enum)
instance Show Quality where
    show Q720P  = "720p"
    show Q1080P = "1080p"
    show Q3D    = "3D"
    show QAll   = "ALL"

data Options = Options { optLimit :: Integer
                       , optSet :: Integer
                       , optQuality :: Quality
                       , optRating :: Rating
                       , optKeywords :: Keywords
                       , optGenre :: Genre
                       , optSort :: Sorting
                       , optOrder :: Order
                       }

startOptions :: Options
startOptions = Options { optLimit = 50
                       , optSet = 1
                       , optQuality = QAll
                       , optRating = 0
                       , optKeywords = []
                       , optGenre = AllGenres
                       , optSort = Date
                       , optOrder = Desc
                       }

getSortingFromString :: String -> Sorting
getSortingFromString s
    | ls == "date"    = Date
    | ls == "seeds"    = Seeds
    | ls == "peers"    = Peers
    | ls == "size"     = Size
    | ls == "alphabet" = Alphabet
    | ls == "rating"   = Rating
    | ls == "download" = Download
    | ls == "year"     = Year
    | otherwise       = Year
    where ls = map toLower s

getOrderFromString :: String -> Order
getOrderFromString s
    | ls == "asc"  = Asc
    | ls == "desc" = Desc
    | otherwise    = Desc
    where ls = map toLower s

getGenreFromString :: String -> Genre
getGenreFromString s
    | ls == "all"  = AllGenres
    | otherwise   = AllGenres
    where ls = map toLower s

getQualityFromString :: String -> Quality
getQualityFromString s
    | ls == "1080p" = Q1080P
    | ls == "720p"  = Q720P
    | ls == "3D"    = Q3D
    | ls == "all"   = QAll
    | otherwise    = QAll
    where ls = map toLower s

getInRange :: Integer -> Integer -> Integer -> Integer
getInRange lower upper n = maximum([lower, minimum([upper, n])])

options :: [OptDescr (Options -> IO Options)]
options = 
    [ Option "l" ["limit"]
        (ReqArg 
            (\arg opt -> do 
                let n = read arg :: Integer
                return opt { optLimit = getInRange 1 50 n })
            "Int between 1 - 50 (inclusive)")
        "Determines the max amount of movie results"
    , Option "p" ["page"]
        (ReqArg 
            (\arg opt -> do
                let n = read arg :: Integer
                return opt { optSet = n })
            "Int")
        "Used to see the next set of movies, eg limit=15 and set=2 will show you movies 15-30"
    , Option "q" ["quality"]
        (ReqArg 
            (\arg opt -> do
                let q = getQualityFromString arg
                return opt { optQuality = q })
            "Enum in [All, 1080p, 720p, 3D]")
        "The quality of the movie"
    , Option "r" ["rating"]
        (ReqArg 
            (\arg opt -> do 
                let n = read arg :: Integer
                return opt { optLimit = getInRange 0 9 n })
            "Int between 0 - 9 (inclusive)")
        "Sets minimum movie rating for display"
    , Option "g" ["genre"]
        (ReqArg 
            (\arg opt -> do 
                let q = getGenreFromString arg
                return opt { optGenre = q })
            "Enum in [...] XXX TODO")
        "Display movies from chosen type genre"
    , Option "s" ["sort"]
        (ReqArg 
            (\arg opt -> do 
                let s = getSortingFromString arg
                return opt { optSort = s })
            "Enum in [Date, Seeds, Peers, Size, Alphabet, Rating, Download, Year]")
        "Property to sort by"
    , Option "o" ["order"]
        (ReqArg 
            (\arg opt -> do 
                let o = getOrderFromString arg
                return opt { optOrder = o })
            "Enum in [Asc, Desc]")
        "The order"
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
            prg <- getProgName
            hPutStrLn stderr (usageInfo prg options)
            exitWith ExitSuccess
            ))
        "Show help"
    ]

q :: (Show b) => String -> (Maybe b) -> QueryItem
q name Nothing           = (BS.pack name, Nothing)::QueryItem
q name (Just v)          = (BS.pack name, Just (BS.pack $ show v))::QueryItem

q' :: String -> String -> QueryItem
q' name value = (BS.pack name, Just (BS.pack value))::QueryItem

-- Create a new list from a search
search :: Action
search args = do
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    let Options { optLimit = limit
                , optSet = set
                , optQuality = quality
                , optRating = rating
                , optGenre = genre
                , optSort = sort
                , optOrder = order
                } = opts

    -- putStrLn ("Limit: " ++ (show limit))
    -- putStrLn ("Set: " ++ (show set))
    -- putStrLn ("Quality: " ++ (show quality))
    -- putStrLn ("Rating: " ++ (show rating))
    -- putStrLn ("Genre: " ++ (show genre))
    -- putStrLn ("Sort: " ++ (show sort))
    -- putStrLn ("Order: " ++ (show order))
    
    let keywords = join " " nonOptions
    print keywords
    -- Create the query string
    let query = [ q "limit" (Just limit)
                , q "set" (Just set)
                , q "quality" (Just quality)
                , q "rating" (Just rating)
                , q "genre" (Just genre)
                , q "sort" (Just sort)
                , q "order" (Just order)
                , q' "keywords" keywords
                ] :: Query

    let queryString = renderQuery False query
    -- print queryString
    let fullUrl = server ++ "list.json?" ++ (BS.unpack queryString)
    response <- simpleHTTP (getRequest fullUrl)
    json <- getResponseBody response
    -- putStrLn json
    writeToCache json

    return()

