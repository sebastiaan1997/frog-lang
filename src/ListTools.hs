module ListTools where
import Debug.Trace ( trace, traceShow )

splitWhen :: Eq a => ((a -> Bool) -> [a] -> [[a]])
splitWhen f (x : xs) | f x = [x] : splitWhen f xs
splitWhen f (x : xs) = (x : head next) : tail next
    where next = splitWhen f xs

splitWhen _ [x] =[[x]]
splitWhen _ [] = [[]]





