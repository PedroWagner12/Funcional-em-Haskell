module GuardsE3 where

-- Exercício E3 — Guardas em Haskell
-- Tudo resolvido com guardas, como pedido.

import Data.Char (isDigit, isLower, isUpper, isSpace)

-- Bloco A — guardas "clássicas"
sign :: (Ord a, Num a) => a -> Int
sign x
  | x < 0     = -1
  | x == 0    = 0
  | otherwise = 1

myAbs :: (Ord a, Num a) => a -> a
myAbs x
  | x < 0     = -x
  | otherwise = x

max3 :: Ord a => a -> a -> a -> a
max3 x y z
  | x >= y && x >= z = x
  | y >= x && y >= z = y
  | otherwise        = z

-- Bloco B — intervalos e faixas
bmiClass :: Double -> String
bmiClass b
  | b < 18.5  = "baixo"
  | b <= 24.9 = "normal"
  | b <= 29.9 = "sobrepeso"
  | otherwise = "obesidade"

grade :: Double -> Char
grade n
  | n >= 9 = 'A'
  | n >= 7 = 'B'
  | n >= 5 = 'C'
  | n >= 3 = 'D'
  | otherwise = 'F'

-- Bloco C — datas e regras
leap :: Int -> Bool
leap y
  | y `mod` 400 == 0 = True
  | y `mod` 100 == 0 = False
  | y `mod` 4   == 0 = True
  | otherwise        = False

-- Imposto progressivo:
-- 0% até 1200; 10% de 1200..2500; 20% acima de 2500
tax :: Double -> Double
tax s
  | s <= 1200  = 0
  | s <= 2500  = (s - 1200) * 0.10
  | otherwise  = (2500 - 1200) * 0.10 + (s - 2500) * 0.20

-- Bloco D — listas & caracteres
safeHead :: [a] -> Maybe a
safeHead xs
  | null xs   = Nothing
  | otherwise = Just (head xs)

charKind :: Char -> String
charKind c
  | isDigit c = "digit"
  | isLower c = "lower"
  | isUpper c = "upper"
  | isSpace c = "space"
  | otherwise = "other"

-- Bloco E — recursão com guardas
myTake :: Int -> [a] -> [a]
myTake n xs
  | n <= 0     = []
  | null xs    = []
  | otherwise  = h : myTake (n-1) t
  where (h:t) = xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs
  | null xs    = []
  | p h        = h : myFilter p t
  | otherwise  =     myFilter p t
  where (h:t) = xs

fizzbuzz :: Int -> String
fizzbuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3  == 0 = "Fizz"
  | n `mod` 5  == 0 = "Buzz"
  | otherwise       = show n
