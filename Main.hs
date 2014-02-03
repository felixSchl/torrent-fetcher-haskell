import System.Environment (getArgs)
import qualified Router as Router

main :: IO()
main = do
    arguments <- getArgs
    Router.execute arguments
    return()
