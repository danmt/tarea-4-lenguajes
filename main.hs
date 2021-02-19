{- V1 -}

group' :: (Eq a) => [a] -> [[a]]
group' []     = []
group' (x:xs) = (x : takeWhile (== x) xs) : group' (dropWhile (== x) xs)

{- V2 -}

group'' :: (Eq a) => [a] -> [[a]]
group'' [] = []
group'' (x:xs) = (x : (fst s)) : group'' (snd s)
  where s = span (== x) xs

{- V3 -}

group''' :: Eq a => [a] -> [[a]]
group''' = foldr f []
  where f x []        = [[x]]
        f x (ys@(y:_):yss)
          | x == y     = (x:ys):yss
          | otherwise  = [x]:ys:yss

{- V4 -}

group'''' :: (Eq a) => [a] -> [[a]]
group'''' = foldr f []
  where f x [] = [[x]]
        f x (y:xs) = if x == (head y) then ((x:y):xs) else ([x]:y:xs)

{- V5 -}

group''''' :: (Eq a) => [a] -> [[a]]
group''''' = foldr f []
  where 
    f x [] = [[x]]
    f x (y:xs) 
      | x == x'   = ((x:y):xs)
      | otherwise = ([x]:y:xs)
      where x' = head y

{- 

## si acumulador es [] -> [[x]]

## si acumulador.length > 0 ->

- obtengo el ultima lista (l) de acumulador
- obtengo el primer elemento (x') del ultima lista (l) del acumulador
  
  ## - si x == x' ->

  - agregar elemento (x) a ultima lista (l)
  - retorno el acumulador, reemplazando el ultimo por el nuevo l

  ## - si x != x' ->

  - agregar lista [x] a acumulador
 -}