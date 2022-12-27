module Part1.Tasks where

import Util(notImplementedYet)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin = notImplementedYet

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos = notImplementedYet

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD x 0 = abs x
myGCD 0 y = abs y
myGCD x y | abs y > abs x = myGCD y x
          | otherwise = myGCD y (mod x y)

-- алгоритм Евклида: gcd (max `mod` min) min

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year | day < 1 || day > 31 || month < 1 || month > 12 || year < 0 = False
                             | day > 30 && (month == 4 || month == 6 || month == 9 || month == 11) = False
                             | month == 2 && day > 29 = False
                             | month == 2 && day > 28 && not leapYear = False
                             | otherwise = True
              where leapYear = mod year 400 == 0 || (mod year 4 == 0 && mod year 100 /= 0)

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x 0 = 1
myPow x n | even n = myPow (x * x) (div n 2)
          | otherwise = x * myPow x (n - 1)

-- алгоритм бинарного возведения в степень      a^n = (a^(n/2))^2 = a^(n/2) * a^(n/2); для четной степени
--                                              a^n = a^(n-1) * a; для нечетной степени

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime k | k > 1 = null [ x | x <- [2..k - 1], mod k x == 0]
          | otherwise = False

-- число простое, если делится на 1 и само себя
-- то есть если найдутся еще делители, то список будет не пустой и число - не простое

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea = notImplementedYet

-- S = 1/2 * |x_1*y_2 + x_2*y_3 + ... + x_(n-1)*y_n + x_n*y_1 + x_2*y_1 - x_3*y_2 - ... - x_n*y_(n-1) - x_1*y_n|
-- n - кол-во сторон многоугольника

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  2, если он тупоугольный
--  0, если он остроугольный
--  1, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind c a b | c > b && b > a = triangleKind c b a
                   | b > c && c > a = triangleKind b c a
                   | b > a && a > c = triangleKind b a c
                   | a > c && c > b = triangleKind a c b
                   | a > b && b > c = triangleKind a b c
                   | c > a + b = -1
                   | c * c == a * a + b * b = 1
                   | c * c < a * a + b * b = 0
                   | c * c > a * a + b * b = 2

-- 1. если треугольник, то должно выполняться неравенство треугольника c < a + b
-- 2. теорема Пифагора - прямоугольный
-- 3. c^2 < a^2 + b^2 - остроугольный
-- 4. c^2 > a^2 + b^2 - тупоугольный
-- необходимо учесть, что c > a > b