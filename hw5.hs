data LTree a = LLeaf a | LNode a (LTree a) (LTree a)
  deriving (Eq,Show)

data MTree a = MLeaf a | UNode a (MTree a) | BNode (MTree a) (MTree a)
  deriving (Eq,Show)


-- Problem 1
getLLeaves :: LTree a -> [a]
getLLeaves (LLeaf x) = [x]
getLLeaves (LNode _ t1 t2) = getLLeaves t1 ++ getLLeaves t2


getMLeaves :: MTree a -> [a]
getMLeaves (MLeaf x) = [x]
getMLeaves (UNode _ t) = getMLeaves t
getMLeaves (BNode t1 t2) = getMLeaves t1 ++ getMLeaves t2


-- Problem 2
maxLDepth :: LTree a -> Integer
maxLDepth (LLeaf _) = 0
maxLDepth (LNode _ t1 t2) = let maxLeft = maxLDepth t1
                                maxRight = maxLDepth t2
                            in if maxLeft > maxRight then 1 + maxLeft
                               else 1 + maxRight

maxMDepth :: MTree a -> Integer
maxMDepth (MLeaf _) = 0
maxMDepth (UNode _ t) = 1 + maxMDepth t
maxMDepth (BNode t1 t2) = let maxLeft = maxMDepth t1
                              maxRight = maxMDepth t2
                          in 1 + max maxLeft maxRight


-- Problem 3
maxLTree :: LTree Integer -> Integer
maxLTree (LLeaf x) = x
maxLTree (LNode x t1 t2) = let maxLeft = maxLTree t1
                               maxRight = maxLTree t2
                           in max x (max maxLeft maxRight)

maxMTree :: MTree Integer -> Integer
maxMTree (MLeaf x) = x
maxMTree (UNode x t) = max x (maxMTree t)
maxMTree (BNode t1 t2) = let maxLeft = maxMTree t1
                             maxRight = maxMTree t2
                         in max maxLeft maxRight


-- Problem 4
uncoveredLeafL :: Integer -> LTree Integer -> Bool
uncoveredLeafL a (LLeaf x) = x == a
uncoveredLeafL a (LNode x t1 t2) = if x == a then False
                                   else uncoveredLeafL a t1 || uncoveredLeafL a t2

uncoveredLeafM :: Integer -> MTree Integer -> Bool
uncoveredLeafM a (MLeaf x) = x == a
uncoveredLeafM a (UNode x t) = if x == a then False
                               else uncoveredLeafM a t
uncoveredLeafM a (BNode t1 t2) = uncoveredLeafM a t1 || uncoveredLeafM a t2


-- Problem 5
mapLTree :: (a -> b) -> LTree a -> LTree b
mapLTree f (LLeaf x) = LLeaf (f x)
mapLTree f (LNode x t1 t2) = LNode (f x) (mapLTree f t1) (mapLTree f t2)

mapMTree :: (a -> b) -> MTree a -> MTree b
mapMTree f (MLeaf x) = MLeaf (f x)
mapMTree f (UNode x t) = UNode (f x) (mapMTree f t)
mapMTree f (BNode t1 t2) = BNode (mapMTree f t1) (mapMTree f t2)


-- Problem 6
-- Need to rewrite this with lambda functions
applyLfun :: LTree Integer -> LTree Integer
applyLfun = mapLTree (\x -> (2 ^ (x*x)) - x)

applyMfun :: MTree Integer -> MTree Integer
applyMfun = mapMTree (\x -> (2 ^ (x*x)) - x)


-- Problem 7
orMaybes :: Maybe a -> Maybe a -> Maybe a
orMaybes Nothing y = y
orMaybes x _ = x

findLTree :: (a -> Bool) -> LTree a -> Maybe a
findLTree f (LLeaf x) | f x = Just x
                      | otherwise = Nothing
findLTree f (LNode x t1 t2) = orMaybes (if (f x) then Just x else Nothing) (orMaybes (findLTree f t1) (findLTree f t2))

findMTree :: (a -> Bool) -> MTree a -> Maybe a
findMTree f (MLeaf x) | f x = Just x
                      | otherwise = Nothing
findMTree f (UNode x t) = orMaybes (if (f x) then Just x else Nothing) (findMTree f t)
findMTree f (BNode t1 t2) = orMaybes (findMTree f t1) (findMTree f t2)


-- Problem 8
findLpali :: LTree String -> Maybe String
findLpali = findLTree isPalindrome
            where isPalindrome a = a == reverse a

findMpali :: MTree String -> Maybe String
findMpali = findMTree isPalindrome
            where isPalindrome a = a == reverse a


-- Problem 9
foldLTree :: (a -> b -> b -> b) -> (a -> b) -> LTree a -> b
foldLTree n l (LLeaf x) = l x
foldLTree n l (LNode x t1 t2) = n x (foldLTree n l t1) (foldLTree n l t2)

foldMTree :: (b -> b -> b) -> (a -> b -> b) -> (a -> b) -> MTree a -> b
foldMTree n u l (MLeaf x) = l x
foldMTree n u l (UNode x t) = u x (foldMTree n u l t)
foldMTree n u l (BNode t1 t2) = n (foldMTree n u l t1) (foldMTree n u l t2)

getLLeaves' :: LTree a -> [a]
getLLeaves' = foldLTree (\_ t1 t2 -> t1 ++ t2) (\x -> [x])

getMLeaves' :: MTree a -> [a]
getMLeaves' = foldMTree (++) (\_ t -> t) (\x -> [x])


-- Problem 10
uncoveredLeafL' :: Integer -> LTree Integer -> Bool
uncoveredLeafL' a = foldLTree (\x t1 t2 -> x /= a && (t1 || t2)) (== a)

uncoveredLeafM' :: Integer -> MTree Integer -> Bool
uncoveredLeafM' a = foldMTree (||) (\x t -> x /= a && t) (== a)
