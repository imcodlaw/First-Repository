  module Bro where

import Data.List
------------------------------------
null' [] = True
null' [_] = False
------------------------------------
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n-1) xs
------------------------------------
drop' _ [] = []
drop' 0 xs = xs
drop' n (s:xs) = drop' (n-1) xs
------------------------------------
fst' (x,y) = x
------------------------------------
snd' (x,y) = y
------------------------------------
--delete' _ [] = []
--delete' x (x:xs) = xs
------------------------------------
--zip' [] [] = []
--zip' [] _ = []
--zip' _ [] = []
--zip' (x:xs) (y:ys) = (x,y)
-----------------
--nth' (x:xs) n =
-----------------
--sort' [] = []
--sort' (x:xs)
-------------------------------------
head' (x:xs) = x
-------------------------------------
length' [] = 0
length' (x:xs) = x + length' xs
-------------------------------------
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = reverse' xs ++ [x]
-------------------------------------
last' [x] = x
last' (x:xs) = last' (xs)
-------------------------------------
tail'(x:xs) = xs
-------------------------------------
init' [x] = []
init' (x:xs) = x : init' xs
-------------------------------------
--concat' [[]] = []
--concat' []
-------------------------------------
sum' [] = 0
sum' [a] = a
sum' (x:xs) = x + sum' xs
-------------------------------------
--product' [] = 1
--product' [0] = [0]
--product' (x:xs) = x * product' xs
-------------------------------------
asal [a,b,c] = "hmmm"