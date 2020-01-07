filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f xs
  = foldr filter' [] xs
  where
    filter' x
      | f x       = (x :)
      | otherwise = id

filterFoldl :: (a -> Bool) -> [a] -> [a]
filterFoldl f xs
  = foldl filter' [] xs
  where
    filter' x y
      | f y       = (y : x)
      | otherwise = id x

inits :: [a] -> [[a]]
inits
  = foldr (\x y -> [] : map (x :) y) [[]]