module Main where

member :: Eq a => a -> [a] -> Bool
member _ [] = False
member x (y:ys) = x == y || member x ys

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- twice [4, 6, 0, 8, -2, 3, 7, 5]  >>> [8, 12, 0, 16, -4, 6, 14, 10]
twice :: Num a => [a] -> [a]
twice [] = []
twice (x:xs) = (2 * x) : twice xs

-- remove-first 'b' ['a', 'b', 'c', 'b', 'd', 'e' ] >>> ['a', 'c', 'b', 'd', 'e']
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y:ys) = if x == y
                        then ys
                        else y : removeFirst x ys

-- allOdd [1, 7, 3, 17, 11] >>> True
-- allOdd [1, 7, 3, 17, 11, 2] >>> False
-- Dica: existe o predicado odd; experimente odd 3  e odd 4
allOdd :: Integral a => [a] -> Bool
allOdd [] = True
allOdd (y:ys) = if odd y
                 then allOdd ys
                else False

-- andMap odd [1, 7, 3, 17, 11] >>> True
-- Dica: é uma adaptação pontual de allOdd
andMap :: (a -> Bool) -> [a] -> Bool
andMap f [] = True
andMap f (y:ys) = if f y
                   then andMap f ys
                   else False

-- some even [1, 7, 3, 17, 11] >>> False
-- some odd [2, 10, 5] >>> True
some :: (a -> Bool) -> [a] -> Bool
some f [] = False
some f (y:ys) = if f y
                 then True
                 else some f ys

-- sum [1, 7, 3, 17, 11]  >>> 39
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (y:ys) = y + sum' ys

-- count 'b' ['a', 'b', 'c', 'b', 'd', 'b', 'e'] >>> 3
count :: Eq a => a -> [a] -> Integer
count _ [] = 0
count x (y:ys) = if x == y
                  then 1+count x ys
                  else count x ys

-- map' succ [10, 20, 30]  >>> [11, 21, 31]
-- Dica: adapte twice
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (y:ys) = f y : map' f ys

-- filter' odd [1, 2, 3, 4] >>> [1, 3]
filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (y:ys) = if f y
                    then y : filter' f ys
                    else filter' f ys

main :: IO ()
main = putStrLn "blah"
