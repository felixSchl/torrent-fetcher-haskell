{-# LANGUAGE OverloadedStrings #-}

module Models (MovieList(..), Movie(..))
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
