module StringUtils (
    joinWith
) where

joinWith :: String -> String -> String
joinWith with str = let split = words str
                        first = head split
                        rest = tail split
                    in first ++ [a ++ b | a <- [with], b <- rest]
