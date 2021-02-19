group' :: (Eq a) => [a] -> [[a]]
group' = foldr f []
  where 
    f x [] = [[x]]
    f x (y:xs) 
      | x == x'   = ((x:y):xs)
      | otherwise = ([x]:y:xs)
      where x' = head y
