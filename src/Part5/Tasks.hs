module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f z [] = z
myFoldl f z (h:t) = myFoldl f (f z h) t

-- пояснение:  myFoldl f a [x,y,z] = ((a `f` x) `f` y) `f` z)

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z [] = z
myFoldr f z (h:t) = f h (myFoldr f z t)

-- пояснение:  myFoldr f a [x,y,z] = x `f` (y `f` (z `f` a))

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f list = myFoldr (\y ys -> (f y):ys) [] list

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f list = myConcat (myMap f list)

myConcat :: [[a]] -> [a]
myConcat list = myFoldr (++) [] list

-- оператор конкатенации ++ принимает в качестве операндов 2 списка и объединяет в 1

myReverse :: [a] -> [a]
myReverse list = myFoldl (\t h -> h:t) [] list

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = myFoldr (\y x -> if p y then y:x else x) [] xs

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p xs = myFoldr (\x (as, bs) -> if p x then (x:as, bs) else (as, x:bs)) ([],[]) xs

-- функция разделяет список на пару списков, в первом находятся те элементы, для которых предикат вернул True,
-- во втором списке - все остальные элементы исходного списка
