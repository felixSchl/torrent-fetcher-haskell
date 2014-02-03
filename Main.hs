
import System.Environment (getArgs)

import Models(Movie(..), MovieList(..))
import Router(exeFromArgs)

main :: IO()
main = do
    arguments <- getArgs
    exeFromArgs arguments
    return()
