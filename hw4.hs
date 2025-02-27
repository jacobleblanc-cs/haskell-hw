--- Section 2 ---


-- Problem 1 --
mapPair :: (a -> b -> c) -> [(a,b)] -> [c]
mapPair f = map (uncurry f)

mapPair' :: (a -> b -> c) -> [(b,a)] -> [c]
mapPair' f = map (uncurry (flip f))


-- Problem 2 --
digitsOnly :: [Integer] -> [Integer]
digitsOnly = filter (\x -> x >= 0 && x <= 9)

removeXs :: [String] -> [String]
removeXs = filter (\s -> not (null s) || head s /= 'X')


-- Problem 3 --
sqLens :: [String] -> [Integer]
sqLens = map (\x -> fromIntegral (length x ^ 2))

bang :: [String] -> [String]
bang = map (++ "!")


-- Problem 4 --
diff :: [Integer] -> [Integer] -> [Integer]
diff = zipWith (-)

splice :: [[String]] -> [[String]] -> [String]
splice xs ys = concat (zipWith (\x y -> x ++ y ++ x) xs ys)


-- Problem 5 --
firstStop :: String -> String
firstStop = takeWhile (/= '.')

boundRange :: Integer -> [Integer] -> [Integer]
boundRange n xs = takeWhile (\x -> abs x <= n) xs




--- Section 3 ---


-- Problem 1
exists :: (a -> Bool) -> [a] -> Bool
exists _ [] = False
exists f (x:xs) = f x || exists f xs

exists' :: (a -> Bool) -> [a] -> Bool
exists' f = foldr (\x acc -> f x || acc) False


-- Problem 2
noDups :: Eq a => [a] -> [a]
noDups (x:xs) = if x `elem` xs then xs else x : noDups xs

noDups' :: Eq a => [a] -> [a]
noDups' = foldr (\x xs -> if x `elem` xs then xs else x:xs) []


-- Problem 3
countOverflow :: Integer -> [String] -> Integer
countOverflow _ [] = 0
countOverflow x (s:ss) = (if fromIntegral (length s) > x then 1 else 0) + countOverflow x ss 
--
--countOverflow' :: Integer -> [String] -> Integer
--countOverflow' x strings = foldr (\s acc -> if length s > (fromIntegral x) then acc + 1 else acc) 0 strings


-- Problem 4
concatList :: [[a]] -> [a]
concatList [] = []
concatList (x:xs) = x ++ concatList xs

concatList' :: [[a]] -> [a]
concatList' = foldr (++) []


-- Problem 5
bindList :: (a -> [b]) -> [a] -> [b]
bindList _ [] = []
bindList f (x:xs) = f x ++ bindList f xs

bindList' :: (a -> [b]) -> [a] -> [b]
bindList' f = foldr (\x acc -> f x ++ acc) []
