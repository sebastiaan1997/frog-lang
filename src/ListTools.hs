module ListTools where
import Debug.Trace ( trace, traceShow )

-- Function that splits a list of type `a` and splits the list when the function returns true at the given element.
splitWhen :: Eq a => ((a -> Bool) -> [a] -> [[a]])
splitWhen f [x] -- If the list contains just a single item, use this function.
    | f x = [[]] -- If the function returns true, return 2 dimentional empty list.
    | otherwise = [[x]] -- If the function returns false, the value should be added to the second dimention.



splitWhen f (x  : xs)   -- If the list contains multiple items, use this function.
    | f x = [] : splitWhen f xs -- If the function returns true; create a new empty list at the front of the main list.
    | otherwise = (x : head next) : tail next -- Append the current value to the front value of the list.
    where next = splitWhen f xs

splitWhen f []=[]


