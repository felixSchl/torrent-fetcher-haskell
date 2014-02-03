module Types(Action, Arguments)
where

type Action = [String] -> IO()
type Arguments = [String]
