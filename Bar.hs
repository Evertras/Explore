module Bar where

data Bar = Bar {
                currentValue :: Int
              , maxValue :: Int
            }

instance Show Bar where
    show health = (show $ currentValue health) ++ "/" ++ (show $ maxValue health)

reduceBy :: Bar -> Int -> Bar
reduceBy h d
        | d < 0 = error "Cannot reduce by negative amounts"
        | otherwise =  h { currentValue = max 0 ((currentValue h) - d) }

replenishBy :: Bar -> Int -> Bar
replenishBy h d
        | d < 0 = error "Cannot replenish negative amounts"
        | otherwise = h { currentValue = min (maxValue h) ((currentValue h) + d) }
