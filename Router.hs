module Router (execute)
where

import Types
import Routes.Download(download)
import Routes.Search(search)
import Routes.Details(details)
import Routes.ShowLast(showlast)

l = wrapAction "show" showlast
d = wrapAction "download" download
s = wrapAction "search" search
i = wrapAction "details" details

execute :: [String] -> IO ()
execute ("list":     args)  = l args
execute ("l":        args)  = l args
execute ("download": args)  = d args
execute ("d":        args)  = d args
execute ("search":   args)  = s args
execute ("s":        args)  = s args
execute ("info":     args)  = i args
execute ("i":        args)  = i args
execute [] = error "No arguments provided"

-- Debug wrapper
wrapAction :: String -> Action -> Arguments -> IO ()
wrapAction name action args = do
    putStrLn $ "Executing " ++ name ++ "..."
    action args

