module Router (execute)
where

import Types
import Routes.Download(download)
import Routes.Search(search)
import Routes.ShowLast(showlast)

-- Routes the input arguments to actions
execute :: [String] -> IO ()
execute ("show":     args) = wrapAction "show" showlast args
execute ("download": args) = wrapAction "download" download args
execute ("search":   args) = wrapAction "search" search args
execute []                 = error "No arguments provided"

-- Debug wrapper
wrapAction :: String -> Action -> Arguments -> IO ()
wrapAction name action args = do
    putStrLn $ "Executing " ++ name ++ "..."
    action args

