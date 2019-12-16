{-
:l /Users/cal/Desktop/haskell/Calculator_the_game.hs
-}

digits :: Int -> [Int]
digits n
    | n < 1     = []
    | otherwise = digit : digits (n - digit * base)
    where power = floor $ (log float / log 10.0)
          base  = 10 ^ power
          digit = n `div` base
          float = fromIntegral n

undigits :: [Int] -> Int
undigits [] = 0
undigits (x:xs) = x*10^(length xs) + undigits xs

data Fn = Fn {fn :: (Int -> Int),
           view :: String}
{-}
solve :: [Int -> Int] -> Int -> Int -> [String]
solve fns start goal
-}
form :: [Int -> Int] -> [Int] -> Int -> [Int]
form fns start goal
     | goal `elem` start = start
     | otherwise         = form fns (fns <*> start) goal

solve :: [Int -> Int] -> Int -> Int -> [Int]
solve fs start goal = reverse $ backtrack (length fs) (length xs) $ indexOf goal xs
     where xs = form fs [start] goal

backtrack :: Int -> Int -> Int -> [Int]
backtrack numFns 1   index = []
backtrack numFns len index = val : backtrack numFns (len `div` numFns) (index `mod` (len `div` numFns))
     where val = index `div` section
           section = len `div` numFns
--the line above this comment is wrong.


-- requires a list that contains the element
indexOf :: (Eq a) => a -> [a] -> Int
indexOf v (x:xs)
     | v == x = 0
     | otherwise = 1 + indexOf v xs

sub :: Int -> Int -> [a] -> [a]
sub low high = take (high - low + 1) . drop low 

inv10 = undigits . map (\x -> if x == 10 then 0 else 10-x) . digits

rev = undigits . reverse . digits

to :: Int -> Int -> Int -> Int
to xs ys n = map (x -> if x == match then replacement else x) . map (take replaceLength) . takeWhile (element -> length element >= replaceLength) . tails num
     where match = digits xs
           replacement = digits ys
           num = digits n
           replaceLength = length replacement



