filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f xs
  = foldr (\x -> if f x then (x :) else id) [] xs

filterFoldl :: (a -> Bool) -> [a] -> [a]
filterFoldl f xs
  = foldl (\x y -> if f y then (y : x) else id x) [] xs

inits :: [a] -> [[a]]
inits
  = foldr (\x y -> [] : map (x :) y) [[]]