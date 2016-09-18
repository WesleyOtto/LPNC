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
-- ==========
--  2a parte

-- stammer ['a', 'b', 'c', 'd']  >>> ['a', 'a', 'b', 'b', 'c', 'c', 'd', 'd']
-- dica: usar o operador : (cons) duas vezes
stammer :: [a] -> [a]
stammer [] = []
stammer (y:ys) = y:y:stammer ys

-- listRef ['a', 'b', 'c', 'd', 'e'] 0 >>> 'a'
-- listRef ['a', 'b', 'c', 'd', 'e'] 3 >>> 'd'
-- dicas: na recursão, a lista e a posição devem "diminuir" juntas;
--        lembre-se que o único elemento visível é o primeiro elemento
listRef :: [a] -> Int -> a
listRef [] _ = undefined
listRef (y:ys) x = if x == 0
                   then y
                   else listRef ys (x-1)

-- listTail ['a', 'b', 'c', 'd', 'e'] 0 >>> ['a', 'b', 'c', 'd', 'e']
-- listTail ['a', 'b', 'c', 'd', 'e'] 3 >>> ['d', 'e']
-- dica: basta uma pequena modificação em listRef
listTail :: [a] -> Int -> [a]
listTail [] _ = []
listTail (y:ys) x = if x == 0
                   then y:ys
                   else listTail ys (x-1)

-- alternate ['a', 'b', 'c', 'd', 'e', 'f'] >>> ['b', 'a', 'd', 'c', 'f', 'e']
-- alternate ['a', 'b', 'c', 'd', 'e', 'f', 'g'] >>> ['b', 'a', 'd', 'c', 'f', 'e', 'g']
-- dicas: acesse os dois primeiros elementos com x:y:xs
--        na recursão, usar xs de x:y:xs faz a lista diminuir dois elementos
--        inclua mais uma condição de parada para listas de um elemento [z]
alternate :: [a] -> [a]
alternate [] = []
alternate [z] = [z]
alternate (x:y:xs) = y:x:alternate xs

-- sorted [10, 13, 45] >>> True
-- sorted [12, 15, 3, 20] >>> False
-- sorted [] >>> True
-- dicas: acesse os dois primeiros elementos com x:y:xs
--       chame a recursão reduzindo apenas um elemento, chamando y:xs
sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:xs) = if x <= y
                  then sorted (y:xs)
                  else False

-- odds ['a', 'b', 'c', 'd', 'e', 'f'] >>> ['b', 'a', 'd', 'c', 'f', 'e']
-- odds ['a', 'b', 'c', 'd', 'e', 'f', 'g'] >>> ['b', 'a', 'd', 'c', 'f', 'e', 'g']
-- dica: adapte alternate
odds :: [a] -> [a]
odds [] = []
odds [z] = [z]
odds (x:y:xs) = x:odds (xs)

-- unique ['a', 'a', 'a', 'b', 'b', 'a', 'a', 'a', 'c', 'c'] >>> ['a', 'b', 'a', 'c']
-- dicas: crie uma função auxiliar que possui um parâmetro a mais
--        sugestão do nome da função auxiliar -> unique'
--        o parâmetro extra leva como informação qual elemento foi suprimido por último
--        a função auxiliar é a real   função a resolver o problema
--        unique é uma fachada para a função auxilidar, menos carregada de informação
--            para quem usa

unique' :: Eq a => [a] -> a -> [a]
unique' [] a = []
unique' (x:xs) a = if x == a
                   then unique' (xs) x
                   else x:unique' (xs) x

unique :: Eq a => [a] -> [a]
unique (x:xs) = x:unique' (xs) x

-- merge [2, 6, 18, 54] [1, 3, 9, 18, 27, 81]  >>> [1, 2, 3, 6, 9, 18, 18, 27, 54, 81]
-- considere que as listas de entrada são ordenadas
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs[] = xs
merge [] ys = ys
merge (x:xs)(y:ys) = if x > y
                     then y: merge (x:xs)(ys)
                     else x:merge (xs)(y:ys)
-- ==========
--  3a parte

-- max [1, 7, 3, 17, 11]  >>> 17
max' :: Ord a => [a] -> a
max' [] = error "it doesn't have elemen"
max' [x] = x
max' (x:y:xs) = if x > y
                then max'(x:xs)
                else max'(y:xs)

-- append [2, 6, 18, 54] [1, 3, 9, 18, 27, 81]  >>> [2, 6, 18, 54, 1, 3, 9, 18, 27, 81]
append :: [a] -> [a] -> [a]
append [] [] = []
append (x:xs)(y:ys) = x:xs ++ y:ys


-- remq 'b' ['a', 'b', 'c', 'b', 'd', 'e' ] >>> ['a', 'c', 'd', 'e']
remq :: Eq a => a -> [a] -> [a]
remq _ [] = []
remq x (y:ys) = if x == y
                  then  remq x ys
                  else y : remq x ys

-- removeKFirst 'a' 2 ['x', 'y', 'a', 'z', 'w', 'a', 'f', 'g', 'a', 'p']  >>> ['x', 'y', 'z', 'w', 'f', 'g', 'a', 'p']
removeKFirst :: Eq a => a -> Int -> [a] -> [a]
removeKFirst _ _ [] = []
removeKFirst x z (y:ys) = if z == 0
                          then y:removeKFirst x z ys
                          else if x == y
                               then removeKFirst x (z-1) ys
                               else y:removeKFirst x z ys


-- longest [[-1, 3], [10,11,12,13], [], [45,55]]  >>> [10, 11, 12, 13]
longest :: [[a]] -> [a]
longest [[]] = []

-- firstOccurrence ['a', 'a', 'a', 'b', 'b', 'a', 'a', 'a', 'c', 'c']  >>> ['a', 'b', 'c']
-- Errado!!!
firstOccurrence :: Eq a => [a] -> [a]
firstOccurrence [] = []
firstOccurrence (x:y:xs) = if x == y
                         then x: firstOccurrence(xs)
                         else x: y: firstOccurrence(xs)

-- substitute 'c' 'd' ['a', 'c', 'c', 'e']  >>> ['a', 'd', 'd', 'e']
substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute z k (x:xs) = if x == z
                        then k: substitute z k xs
                        else x: substitute z k xs

-- parlis [1, 2, 3] [11, 12, 13]  >>> [[1, 11], [2, 12], [3, 13]]
pairlis :: [a] -> [a] -> [[a]]
pairlis [] [] = [[]]

-- pos+ [7, 5, 1, 4]    >>> [7, 6, 3, 7]
-- somar o elemento com sua posição
-- Inicia a com 0
posPlus :: Int -> [Int] -> [Int]
posPlus _ [] = []
posPlus y (x:xs) = (x+y): posPlus (y+1) xs

main :: IO ()
main = putStrLn "blah"
