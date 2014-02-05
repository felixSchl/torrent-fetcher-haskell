{-# LANGUAGE OverloadedStrings #-}

module Models (MovieList(..), Movie(..), MovieDetails(..), getMovieById)
where

import Data.Aeson ((.:), (.:?), FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))

-- A list of Movies
data MovieList = MovieList { getMovies :: [Movie]
                           }
                           deriving (Show)
instance FromJSON MovieList where
    parseJSON (Object v) =
        MovieList <$>
            (v .: "MovieList")

getMovieById :: MovieList -> String -> Maybe Movie
getMovieById ml id = do
    let movies = getMovies ml
    let movies' = filter (\m -> getMovieID m == id) movies
    if length movies' > 0 then Just(head movies') else Nothing

-- A single Movie
data Movie = Movie { getMovieID :: String
                   , getMovieTitle :: String
                   }
                   deriving (Show)
instance FromJSON Movie where
    parseJSON (Object v) =
        Movie <$>
            (v .: "MovieID")    <*>
            (v .: "MovieTitle")

-- Movie Details
data MovieDetails = MovieDetails { getMovieUrl :: String
                                 , getShortDescription :: String
                                 }
                                 deriving (Show)
instance FromJSON MovieDetails where
    parseJSON (Object v) =
        MovieDetails <$>
            (v .: "MovieUrl")    <*>
            (v .: "ShortDescription")
