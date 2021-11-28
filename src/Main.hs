module Main where

pys n = [(a, b, c(a)(b)) | a <- [1 .. n], b <- [a+1 .. n], (a<n) && (b<n) && (c(a)(b) < n) && isInt (c(a)(b))]

isInt x = x == fromInteger (round x)
c a b = sqrt(a^2+b^2)

lastn :: [Int] -> Int -> [Int]
lastn l n = drop ((length l) - n) (l)

arith :: [Int] -> Int
arith l =  div (sum l) (length l)

anzg :: [Int] -> Int
anzg l = length (filter (even) l)

sumq :: [Int] -> Int
sumq l = sum (map (^2) l)

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

-- sort l = sort' l []
-- sort' [] acc = acc
-- sort' l acc = sort' (even l) (acc ++ (smallestElement l))

-- smallestElement l = smallestElement' l (head l) where
-- smallestElement' [] acc = acc
-- smallestElement' l acc = smallestElement' (drop 1 l) (if' (acc<head l) acc (head l))

-- removeSmallestElement l = removeSmallestElement' l 0 where
-- removeSmallestElement' index | (l !! index) == (smallestElement l) = removeElementAt 
-- l index
-- | othrewise = removeSmallestElement' (index+1)

-- work
-- removeElementAt l index = l

main = do
 putStrLn ""

if' True  x y = x
if' False x y = y

-- multi :: Float -> Float -> Float
multi x y = x*y

-- Aufgabe 1
----
-- cubic :: Float -> Float
cubic x = x^3

-- cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume x y z = x*y*z

-- cuboidSurface :: Float -> Float -> Float -> Float
cuboidSurface x y z = x*y+x*z*y*z

--
-- triangleSurface :: Float -> Float -> Float
triangleSurface b hB = if' ((b < 0) || (hB < 0)) (-1.0) (b*hB/2)
----

-- Aufgabe 1, 20.08.
----
-- add5 :: Float -> Float
add5 = (+) 5

-- pow3 :: Float -> Float
pow3 x = x^3

-- inc :: Float -> Float
inc = (+) 1

-- isTwoDigits :: Float -> Bool
isTwoDigits x = (abs x) >= 10.0 && (abs x) < 100

isTwoDigitsVer x | (isTwoDigits x) = True
                 | otherwise = False
----

-- Aufgabe 2
----

--add2 :: Int -> Int 
add2 = (+) 2 

-- one :: Int 
one = 1

-- cnst :: Int -- cnst = 11
cnst = add2 (add2 (add2 (add2 (add2 one))))

-- add6 :: Int -> Int -- add = (+) 6
add6 x = add2 (add2 (add2 x))
----

-- Recursion
----

-- mul :: Float -> Int -> Float
mul x 0 = 0
mul x y = x + (mul x (y - 1))

-- pow :: Float -> Int -> Float
pow b 0 = 0
pow b e = b * (mul b (e - 1))

-- fac :: Int -> Int
fac 0 = 1
fac x = x * fac (x-1)

facG x | (x == 0) = 0
      | otherwise = x * (fac (x - 1))

-- mods :: Int -> Int -> Int
mods x y = if' ((x - y) == 0) (0) (if' ((x - y) > 0) (mods (x - y) y) (x))

modsV2 x y | ((x - y) == 0) = 0
          | ((x - y) > 0) = (modsV2 (x - y) y)
          | otherwise = x

modsV3 x y = if ((x - y) == 0) then (0) else if ((x - y) > 0) then (modsV3 (x - y) y)
          else x

-- divs :: Int -> Int -> Int
divs x y = (x - (mods x y)) / y

-- ggT :: Int -> Int -> Int
ggT x y = if' (y > 0) (ggT y (mods x y)) (x)

-- // TypeScript
-- function ggT(x: number, y: number) {
--  let z: number;
--  while (y > 0) {
--    z = x % y;
--    x = y;
--    y = r;
--  }
--  return x;
-- }

-- evens :: Int -> Boolean
evens x = (mods x 2) == 0

-- odds :: Int -> Boolean
odds x = (mods x 2) == 1

-- aemul :: Int -> Int  -> Int 
-- wenn x gerade ist (Teiler 2 ist vorhanden)
-- dann: aemul (x/2) (y*2)
-- else y + aemul ((x-1)/2) (y*2)
-- der base case ist a == 0, welches 0 returnt
-- wir rechnen x * y
aemul x y | (x == 0) = 0
          | otherwise = if' (evens x) (aemul (divs x 2) (2 * y)) (y + aemul (divs (x - 1) 2) (2 * y))
----

-- fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

-- fib 5 => fib 4 + fib 3 => fib 3 + fib 2 + fib 2 + fib 1 
-- => fib 2 + fib 1 + fib 1 + fib 0 + fib 1 + fib 0 + 1
-- => fib 1 + fib 0 + 1 + 1 + 1 + 1 + 1 + 1
-- => 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 = 8

-- fib 30 braucht mehr als 10sec im Durchschnitt auf dieser Server Hardware

-- ack :: Int -> Int -> Int
ack 0 y = y + 1
ack x y = if' (x > 0 && y == 0) (ack (x - 1) 1) (if' (x > 0) (ack (x - 1) (ack x (y - 1)) ) (-1))

-- stellenzahl :: Int -> Int
stellenzahl x = digitCount x 0 where
digitCount x counter = if' ((div x 10) == 0) (counter + 1) (digitCount (div x 10) (counter + 1))

-- quersumme :: Int -> Int
quersumme x = querSum x 0
querSum x temp | x == 0 = temp
               | (stellenzahl x) == 1 = temp + x
               | otherwise = (querSum (div (x - (mod x 10)) 10) (temp + (mod x 10)))

-- binom :: Float -> Float -> Float
binom 0 k = if' (k > 0) (-1) 1
binom n 0 = 1
binom n k | (k < 0 || n < 0) || (n < k) = (-1)
          | otherwise = (n * (binom (n - 1) (k - 1))) / k

st :: Int -> Int
st 0 = 0
st x = 1 + st (div x 10)

qs :: Int -> Int
qs 0 = 0
qs x = (mod x 10) + qs (x `div` 10)

b x y = x `binom` y

summ x = summ' x 0 where
summ' 0 akk = akk
summ' x akk = summ' (x-1) (akk+x)

mull x = mull' x 1 where
mull' 0 akk = akk
mull' x akk = mull' (x-1) (akk*x)

multis x y = adds x y 0 where
adds 0 y acc = acc
adds x y acc = adds (x-1) y acc + y

rand :: Int -> Int
rand n = if' (n<3) (n+1) (mod (1+(rand(n-1)-rand(n-2)*rand(n-3))) (100))

randAcc n = randAcc' n 0 where
randAcc' n acc = if' (n<3) (n+1) (comp n acc) where
comp n acc = mod (1+(rand(n-1)-rand(n-2)*rand(n-3))) (100)

isbnP1 0 = 0
isbnP1 x = (11 - (st x)) * (getFirstDigit x) + isbnP1 (x - (getFirstDigit x) * 10^((st x) - 1))

isbn10Checker n = (mod (isbnP1 n) 11) == 0

getFirstDigit n = (div n (10^((st n) - 1)))

isbn13 x = ((mod (isbn13P1 x) 10) == 0) where
isbn13P1 0 = 0 
isbn13P1 x = (3^(1 - (st x `mod` 2))) * (getFirstDigit x) + isbn13P1 (x - (getFirstDigit x) * 10^((st x) - 1))