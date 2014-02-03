module Router (exeFromArgs)
where

import Types
import Routes.Download(download)
import Routes.Search(search)
import Routes.ShowLast(showlast)

-- Routes the input arguments to actions
exeFromArgs :: [String] -> IO ()
exeFromArgs ("show":     args) = wrapAction "show" showlast args
exeFromArgs ("download": args) = wrapAction "download" download args
exeFromArgs ("search":   args) = wrapAction "search" search args
exeFromArgs []                 = error "No arguments provided"

-- Debug wrapper
wrapAction :: String -> Action -> Arguments -> IO ()
wrapAction name action args = do
    putStrLn $ "Executing " ++ name ++ "..."
    action args

