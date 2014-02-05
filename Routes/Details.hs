module Routes.Details
where

import Config
import Types
import Models(MovieDetails)
import Network.HTTP(getRequest, getResponseBody, simpleHTTP)

details :: Action
details (id:args) = do
    let fullUrl = server ++ "movie.json?id=" ++ id
    response <- simpleHTTP (getRequest fullUrl)
    json <- getResponseBody response

    print json

details args = do
    error "No id specified"
