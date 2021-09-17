{--Split an array equally into two sub-arrays--}

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs) where n = (length xs) `div` 2

{--Find the Euclidean distance between two 2D points--}

euclidean_distance ::  (Int, Int) -> (Int, Int) -> Float
euclidean_distance (x1, y1) (x2, y2) = sqrt (fromIntegral ((x2-x1)^2 + (y2-y1)^2))

{--Return the first word of a sentence--}

first_word:: String -> String
first_word s = s_!!0 where s_ = words s

{--Return the last element of a list (or empty set if the input list is empty).--}

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = drop 1 xs
