-- VERSAO COMPLETA

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

-- removeFirst 'b' ['a', 'b', 'c', 'b', 'd', 'e' ] >>> ['a', 'c', 'b', 'd', 'e']
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y:ys) = if x == y
                        then ys
                        else y : removeFirst x ys

-- allOdd [1, 7, 3, 17, 11] >>> True
-- allOdd [1, 7, 3, 17, 11, 2] >>> False
-- Dica: existe o predicado odd; experimente odd 3  e odd 4
allOdd :: Integral a => [a] -> Bool
allOdd = undefined

-- andMap odd [1, 7, 3, 17, 11] >>> True
-- andMap even [2, 6, 10, 17, 0] >>> False
-- Dica: é uma adaptação pontual de allOdd
andMap :: (a -> Bool) -> [a] -> Bool
andMap = undefined

-- some even [1, 7, 3, 17, 11] >>> False
-- some odd [2, 10, 5] >>> True
some :: (a -> Bool) -> [a] -> Bool
some = undefined

-- sum [1, 7, 3, 17, 11]  >>> 39
sum' :: Num a => [a] -> a
sum' = undefined

-- count 'b' ['a', 'b', 'c', 'b', 'd', 'b', 'e'] >>> 3
count :: Eq a => a -> [a] -> Integer
count = undefined

-- map' succ [10, 20, 30]  >>> [11, 21, 31]
-- Dica: adapte twice
map' :: (a -> b) -> [a] -> [b]
map' = undefined

-- filter' odd [1, 2, 3, 4] >>> [1, 3]
filter' :: (a -> Bool) -> [a] -> [a]
filter' = undefined

-- ==========
--  2a parte

-- stammer ['a', 'b', 'c', 'd']  >>> ['a', 'a', 'b', 'b', 'c', 'c', 'd', 'd']
-- dica: usar o operador : (cons) duas vezes
stammer :: [a] -> [a]
stammer =  undefined

-- listRef ['a', 'b', 'c', 'd', 'e'] 0 >>> 'a'
-- listRef ['a', 'b', 'c', 'd', 'e'] 3 >>> 'd'
-- dicas: na recursão, a lista e a posição devem "diminuir" juntas;
--        lembre-se que o único elemento visível é o primeiro elemento
listRef :: [a] -> Int -> a
listRef = undefined

-- listTail ['a', 'b', 'c', 'd', 'e'] 0 >>> ['a', 'b', 'c', 'd', 'e']
-- listTail ['a', 'b', 'c', 'd', 'e'] 3 >>> ['d', 'e']
-- dica: basta uma pequena modificação em listRef
listTail :: [a] -> Int -> [a]
listTail = undefined

-- alternate ['a', 'b', 'c', 'd', 'e', 'f'] >>> ['b', 'a', 'd', 'c', 'f', 'e']
-- alternate ['a', 'b', 'c', 'd', 'e', 'f', 'g'] >>> ['b', 'a', 'd', 'c', 'f', 'e', 'g']
-- dicas: acesse os dois primeiros elementos com x:y:xs
--        na recursão, usar xs de x:y:xs faz a lista diminuir dois elementos
--        inclua mais uma condição de parada para listas de um elemento [z]
alternate :: [a] -> [a]
alternate = undefined

-- sorted [10, 13, 45] >>> True
-- sorted [12, 15, 3, 20] >>> False
-- sorted [] >>> True
-- dicas: acesse os dois primeiros elementos com x:y:xs
--       chame a recursão reduzindo apenas um elemento, chamando y:xs
sorted :: Ord a => [a] -> Bool
sorted = undefined

-- odds ['a', 'b', 'c', 'd', 'e', 'f'] >>> ['b', 'a', 'd', 'c', 'f', 'e']
-- odds ['a', 'b', 'c', 'd', 'e', 'f', 'g'] >>> ['b', 'a', 'd', 'c', 'f', 'e', 'g']
-- dica: adapte alternate
odds :: [a] -> [a]
odds = undefined

-- unique ['a', 'a', 'a', 'b', 'b', 'a', 'a', 'a', 'c', 'c'] >>> ['a', 'b', 'a', 'c']
-- dicas: crie uma função auxiliar que possui um parâmetro a mais
--        sugestão do nome da função auxiliar -> unique'
--        o parâmetro extra leva como informação qual elemento foi suprimido por último
--        a função auxiliar é a real função a resolver o problema
--        unique é uma fachada para a função auxilidar, menos carregada de informação
--            para quem usa
unique :: Eq a => [a] -> [a]
unique = undefined

-- merge [2, 6, 18, 54] [1, 3, 9, 18, 27, 81]  >>> [1, 2, 3, 6, 9, 18, 18, 27, 54, 81]
-- considere que as listas de entrada são ordenadas
merge :: (Ord a) => [a] -> [a] -> [a]
merge = undefined

-- ==========
--  3a parte

-- max [1, 7, 3, 17, 11]  >>> 17
max :: Ord a => [a] -> a
max = undefined

-- append [2, 6, 18, 54] [1, 3, 9, 18, 27, 81]  >>> [2, 6, 18, 54, 1, 3, 9, 18, 27, 81]
append :: [a] -> [a] -> [a]
append = undefined

-- remq 'b' ['a', 'b', 'c', 'b', 'd', 'e' ] >>> ['a', 'c', 'd', 'e']
remq :: Eq a => a -> [a] -> [a]
remq = undefined

-- removeKFirst 'a' 2 ['x', 'y', 'a', 'z', 'w', 'a', 'f', 'g', 'a', 'p']  >>> ['x', 'y', 'z', 'w', 'f', 'g', 'a', 'p']
removeKFirst :: Eq a => a -> Int -> [a] -> [a]
removeKFirst = undefined

-- longest [[-1, 3], [10,11,12,13], [], [45,55]]  >>> [10, 11, 12, 13]
longest :: [[a]] -> [a]
longest = undefined

-- firstOccurrence ['a', 'a', 'a', 'b', 'b', 'a', 'a', 'a', 'c', 'c']  >>> ['a', 'b', 'c']
firstOccurrence :: Eq a => [a] -> [a]
firstOccurrence = undefined

-- substitute 'c' 'd' ['a', 'c', 'c', 'e']  >>> ['a', 'd', 'd', 'e']
substitute :: Eq a => a -> a -> [a] -> [a]
substitute = undefined

-- parlis [1, 2, 3] [11, 12, 13]  >>> [[1, 11], [2, 12], [3, 13]]
pairlis :: [a] -> [a] -> [[a]]
pairlis = undefined

-- pos+ [7, 5, 1, 4]    >>> [7, 6, 3, 7]
-- somar o elemento com sua posição
pos+ :: [Int] -> [Int]
pos+ = undefined

-- isSuffix [1, 2, 3] [4, 3, 2, 1, 2, 3]  >>> True
-- isSuffix [1, 2, 3] [4, 3, 2, 1, 2]   >>> False
isSuffix ::[a] -> [a] -> [a]
commonSuffix = undefined

main :: IO ()
main = putStrLn "blah"
