module Routes.Search (search)
where

import Config
import Types
import Network.HTTP(simpleHTTP, getRequest, getResponseBody)
import Config(cacheFilename, server)
import Util(writeToCache)

-- Create a new list from a search
search :: Action
search args = do
    response <- simpleHTTP (getRequest (server ++ "list.json"))
    json <- getResponseBody response
    writeToCache json
    return()

