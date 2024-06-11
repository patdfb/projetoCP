{- -- Import necessary modules
import Data.List (foldl')
import Data.Maybe (fromMaybe, listToMaybe)

-- Function to compute the largest rectangle in histogram
lrh :: [Int] -> Int
lrh [] = 0
lrh hs = maximumArea hs 0 (length hs - 1)

-- Helper function to find the minimum index in the given range
minIndex :: [Int] -> Int -> Int -> Int
minIndex hs l r = snd $ minimum [(hs !! i, i) | i <- [l..r]]

-- Function to compute the largest rectangle using divide-and-conquer
maximumArea :: [Int] -> Int -> Int -> Int
maximumArea hs l r
  | l > r = 0
  | otherwise = maximum [leftMax, rightMax, crossMax]
  where
    minIdx = minIndex hs l r
    leftMax = maximumArea hs l (minIdx - 1)
    rightMax = maximumArea hs (minIdx + 1) r
    crossMax = (r - l + 1) * (hs !! minIdx)


numero 2

import Data.List

type Range = (Int, Int)
type Histogram = [Int]

-- Unfold function to generate subproblems
unfold :: Histogram -> [Range]
unfold hs = unfold' 0 (length hs - 1)
  where
    unfold' :: Int -> Int -> [Range]
    unfold' l r
      | l > r     = []
      | otherwise = (l, r) : (unfold' l (m - 1) ++ unfold' (m + 1) r)
      where
        m = minIndex hs l r

-- Helper function to find the index of the minimum element in the range [l, r]
minIndex :: Histogram -> Int -> Int -> Int
minIndex hs l r = snd $ minimum [(hs !! i, i) | i <- [l..r]]

-- Fold function to calculate the largest rectangle area from the subproblems
fold :: Histogram -> [Range] -> Int
fold hs = foldl' max 0 . map (rectArea hs)

-- Helper function to calculate the area of the rectangle defined by a range
rectArea :: Histogram -> Range -> Int
rectArea hs (l, r)
  | l > r     = 0
  | otherwise = (r - l + 1) * hs !! (minIndex hs l r)


-- Function to calculate the largest rectangle area in the histogram
lrh :: [Int] -> Int
lrh hs = fold hs (unfold hs)

numero 3

import Data.List (foldl', minimumBy)
import Data.Ord (comparing)
import Cp

type Range = (Int, Int)
type Histogram = [Int]

-- Function to compute the largest rectangle in histogram
lrh :: [Int] -> Int
lrh hs = fold hs (unfold hs)

-- Unfold function to generate subproblems
unfold :: Histogram -> [Range]
unfold hs = unfold' 0 (length hs - 1)
  where
    unfold' :: Int -> Int -> [Range]
    unfold' l r
      | l > r     = []
      | otherwise = (l, r) : (unfold' l (m - 1) ++ unfold' (m + 1) r)
      where
        m = minIndex hs l r

-- Simplified helper function to find the index of the minimum element in the given range
minIndex :: [Int] -> Int -> Int -> Int
minIndex hs l r = p2 (minimumBy (comparing p1) (zip (take (r - l + 1) (drop l hs)) [l..r]))

-- Fold function to calculate the largest rectangle area from the subproblems
fold :: Histogram -> [Range] -> Int
fold hs = foldl' max 0 . map (rectArea hs)

-- Helper function to calculate the area of the rectangle defined by a range
rectArea :: Histogram -> Range -> Int
rectArea hs (l, r)
  | l > r     = 0
  | otherwise = (r - l + 1) * (p1 (minimumBy (comparing p1) (zip (take (r - l + 1) (drop l hs)) [l..r])))

--numero 4

data Histogram a = Empty | Bar a (Histogram a) deriving (Show, Eq)

-- Definindo o tipo indutivo para representar o histograma

-- Função lrh que calcula a maior área de um retângulo em um  ograma
lrh :: [Int] -> Int
lrh = histoMaxArea . buildHistogram

-- Anamorfismo para construir o tipo indutivo Histogram a partir de uma lista
buildHistogram :: [Int] -> Histogram Int
buildHistogram = foldr Bar Empty

-- Catamorfismo para calcular a maior área do retângulo em um histograma
histoMaxArea :: Histogram Int -> Int
histoMaxArea = cata maxAreaAlg

-- Catamorfismo genérico
cata :: (Histogram a -> Int -> Int) -> Histogram a -> Int
cata _ Empty = 0
cata f (Bar x xs) = f (Bar x xs) (cata f xs)

-- Algoritmo para o catamorfismo
maxAreaAlg :: Histogram Int -> Int -> Int
maxAreaAlg Empty _ = 0
maxAreaAlg (Bar h hs) maxAreaTail = max (h * (1 + lengthHistogram hs)) maxAreaTail

-- Função auxiliar para calcular o comprimento do histograma
lengthHistogram :: Histogram Int -> Int
lengthHistogram Empty = 0
lengthHistogram (Bar _ hs) = 1 + lengthHistogram hs

--numero 5

-- Definição do tipo indutivo para o histograma
data Histogram a = Empty | Bar a (Histogram a) deriving (Show, Eq)

-- Anamorfismo: Função auxiliar para construir o histograma a partir de uma lista
toHistogram :: [a] -> Histogram a
toHistogram []     = Empty
toHistogram (x:xs) = Bar x (toHistogram xs)

-- Catamorfismo: Função que calcula a área do maior retângulo
largestRectangleArea :: Histogram Int -> Int
largestRectangleArea hist = go hist [] 0 0
  where
    go :: Histogram Int -> [(Int, Int)] -> Int -> Int -> Int
    go Empty stack maxArea _ = foldl (\acc (height, start) -> max acc (height * (length heights - start))) maxArea stack
      where heights = map fst stack
    go (Bar h rest) stack maxArea index = go rest newStack newMaxArea (index + 1)
      where
        (newStack, newMaxArea) = clearStack h index stack maxArea

    clearStack :: Int -> Int -> [(Int, Int)] -> Int -> ([(Int, Int)], Int)
    clearStack height index [] maxArea = ([(height, index)], maxArea)
    clearStack height index ((h, start):stack) maxArea
      | h <= height = ((height, index) : (h, start) : stack, maxArea)
      | otherwise = clearStack height index stack (max maxArea (h * (index - start)))

-- Hilomorfismo: Combinação do anamorfismo e do catamorfismo
lrh :: [Int] -> Int
lrh heights = largestRectangleArea (toHistogram heights)

numero 6

import Data.List (tails)

-- Função principal lrh
lrh :: [Int] -> Int
lrh = hylo areaMaxima dividir

-- Hilomorfismo
hylo :: ([Int] -> Int) -> ([Int] -> [[Int]]) -> [Int] -> Int
hylo f g x = maximum (map f (g x))

-- Função que divide a lista em todas as suas sublistas possíveis
dividir :: [Int] -> [[Int]]
dividir xs = concatMap inits (tails xs)
  where
    inits = scanl (flip (:)) []

-- Função que calcula a área máxima para uma dada sublista
areaMaxima :: [Int] -> Int
areaMaxima xs = maximum [h * w | (h, w) <- zip xs [1..]]

-- Exemplo de uso:
-- lrh [2,1,5,6,2,3]

-}

{-

-- (1) Datatype definition -----------------------------------------------------

--- Haskell lists are already defined, so the following is a dummy, informal declaration:
--- data [a] = [] | (a : [a])

inList = either nil cons

outList []    = i1 ()
outList (a:x) = i2(a,x)

baseList g f = id -|- g >< f

-- (2) Ana + cata + hylo -------------------------------------------------------

recList  f   = id -|- id >< f                   -- this is F f for this data type

cataList g   = g . recList (cataList g) . outList   

anaList  g   = inList . recList (anaList g) . g

hyloList f g = cataList f . anaList g

-}

{-
--resolução 7

import Cp
import List

lrh :: [Int] -> Int
lrh = hyloList catamorfismo anamorfismo 

anamorfismo :: [Int] -> [Either () ([Int], [Int])]
anamorfismo [] = [Left ()]
anamorfismo (h:t) = Right (takeWhile (>= h) (h:t), dropWhile (>= h) (h:t)) : anamorfismo t

catamorfismo :: [Either () ([Int], [Int])] -> Int
catamorfismo [] = 0
catamorfismo ((Left ()):xs) = catamorfismo xs
catamorfismo ((Right (l, r)):xs) = maximum (areaRectangles (l, r) : catamorfismo xs)
    where
        areaRectangles (l, r) = [calculateArea (l ++ r) i j | i <- [0..length l - 1], j <- [i..length l - 1]]

calculateArea :: [Int] -> Int -> Int -> Int
calculateArea heights i j = min_height * (j - i + 1)
    where
        min_height = minimum (take (j - i + 1) (drop i heights))

-}


-----------------------
{-
generateSublists :: [Int] -> [([Int], (Int, Int))]
generateSublists heights = [(take (j - i + 1) (drop i heights), (i, j)) | i <- [0..n-1], j <- [i..n-1]]
  where
    n = length heights

calculateAreas :: [([Int], (Int, Int))] -> [Int]
calculateAreas = map (\(heights, (i, j)) -> calculateArea heights i j)

calculateArea :: [Int] -> Int -> Int -> Int
calculateArea heights i j = min_height * (j - i + 1)
  where
    min_height = minimum heights

lrh :: [Int] -> Int
lrh heights = maximum $ calculateAreas $ generateSublists heights

-----outra

geraListas :: [Int] -> [([Int], (Int, Int))]
geraListas heights = [(take (j - i + 1) (drop i heights), (i, j)) | i <- [0..n-1], j <- [i..n-1]]
  where
    n = length heights

calculaAreas :: [([Int], (Int, Int))] -> [Int]
calculaAreas = map (\(heights, (i, j)) -> calculaArea heights i j)

calculaArea :: [Int] -> Int -> Int -> Int
calculaArea heights i j = min_height * (j - i + 1)
  where
    min_height = minimum heights

lrh :: [Int] -> Int
lrh = maximum . calculaAreas . geraListas

-----outra

import Cp
import List

generateRectangles :: [Int] -> Either () ([Int], (Int, Int))
generateRectangles [] = Left ()
generateRectangles heights = Right (take (j - i + 1) (drop i heights), (i, j))
  where
    n = length heights
    (i, j) = (0, n - 1)  

processRectangles :: Either () ([Int], (Int, Int)) -> Int
processRectangles = either (const 0) (\(heights, (i, j)) -> calculateArea heights i j)
  where
    calculateArea heights i j = min_height * (j - i + 1)
      where
        min_height = minimum heights

largestRectangle :: [Int] -> Int
largestRectangle = hyloList processRectangles generateRectangles

--------outra

⁠ -- Definição do tipo indutivo Hist
data Hist a = Empty | Node a (Hist a) (Hist a) deriving (Show, Eq)

-- Anamorfismo: constrói a árvore binária a partir da lista de alturas
ana :: [Int] -> Hist Int
ana [] = Empty
ana xs = Node minVal (ana left) (ana right)
  where
    (left, minVal:right) = break (== minimum xs) xs

-- Catamorfismo: calcula a maior área de um retângulo na árvore binária
cata :: Hist Int -> Int
cata Empty = 0
cata (Node val left right) = max (val * size) (max (cata left) (cata right))
  where
    size = sizeTree left + sizeTree right + 1

-- Função auxiliar para calcular o tamanho de uma árvore
sizeTree :: Hist a -> Int
sizeTree Empty = 0
sizeTree (Node _ left right) = 1 + sizeTree left + sizeTree right

-- Função principal que compõe o anamorfismo e o catamorfismo
lrh :: [Int] -> Int
lrh = cata . ana ⁠

----- não funciona esta

import Cp
import Data.List
import List
import Nat hiding (fac)

-- Decomposition function g
g :: [Int] -> Either () ([Int], [Int])
g [] = i1 ()
g (x:xs) = i2 ([x], xs)

-- Combination function f
f :: Either () ([Int], [Int]) -> Int
f (Left ()) = 0
f (Right (xs, ys)) = calculaArea (xs ++ ys)

-- Calculate area for a list of heights
calculaArea :: [Int] -> Int
calculaArea [] = 0
calculaArea heights = minimum heights * length heights

-- Generate all sublists
allSublists :: [Int] -> [[Int]]
allSublists heights = concatMap inits (tails heights)

-- LRH function using hyloList
lrh :: [Int] -> Int
lrh heights = maximum (map (hyloList f g) (allSublists heights))

-}

-- RESPOSTA CERTA YAY

import Cp
import List
import Data.List

import Data.List (inits, tails)

import Data.List (inits, tails)

import Data.List (inits, tails)

a :: [[Int]] -> Either () (Int, [[Int]])
a [] = i1 ()
a (x:xs) = i2 (calculaArea x, xs)

c :: Either () (Int, Int) -> Int
c (Left ()) = 0
c (Right (xs, ys)) = max xs ys

calculaArea :: [Int] -> Int
calculaArea [] = 0
calculaArea heights = minimum heights * length heights

geraListas :: [Int] -> [[Int]]
geraListas heights = concatMap inits (tails heights)

lrh :: [Int] -> Int
lrh = hyloList c a . geraListas



----------------------------- resposta certa mas sem hilomorfismo ------------------------------------

{-

lrh [ ] = 0
lrh heights = maximum · map calculaArea · geraListas $ heights
geraListas :: [Int] → [[Int]]
geraListas heights = concatMap inits (tails heights)
calculaArea :: [Int] → Int
calculaArea [ ] = 0
calculaArea heights = minimum heights ∗ length heights

-}