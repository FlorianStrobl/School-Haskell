module Main where

main = do
 putStrLn ""

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
if' True  x y = x
if' False x y = y
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

-- mul Float -> Int -> Float
mul x 0 = 0
mul x y = x + (mul x (y - 1))

-- pow Float -> Int -> Float
pow b 0 = 0
pow b e = b * (mul b (e - 1))

----