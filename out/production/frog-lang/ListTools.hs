module ListTools where
import Debug.Trace ( trace, traceShow )

splitWhen :: Eq a => ((a -> Bool) -> [a] -> [[a]])

splitWhen f [x]
    | f x = [[]]
    | otherwise = [[x]]



splitWhen f (x  : xs) 
    | f x = [] : splitWhen f xs
    | otherwise = (x : head next) : tail next
    where next = splitWhen f xs

splitWhen f []=[]


