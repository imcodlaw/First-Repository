module Bro where

import Data.List
--to do list : foldl, fold1, scanl, scanl1
-- to do list : words, lines, union, maximum, minimum, sort, nub, inits, tails
-- intersect, group, splitAt, partition
--___________________________________
null' [] = True
null' _ = False
--___________________________________
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n-1) xs
--___________________________________
drop' _ [] = []
drop' 0 xs = xs
drop' n (s:xs) = drop' (n-1) xs
--___________________________________
fst' (x,y) = x
--___________________________________
snd' (x,y) = y
--___________________________________
map' fx [] = []
map' fx (x:xs) = [fx x] ++ map' fx xs
--___________________________________
tambah x = x + x > 100
-- fungsi tambah buat ngetest filter
--___________________________________
filter' fx [] = []
filter' fx (x:xs)
  | fx x == True = [x] ++ filter' fx xs
  | otherwise = [] ++ filter' fx xs
--___________________________________
delete' _ [] = []
delete' y (x:xs)
  | y == x = xs
  | y /= x = [x] ++ delete' y (xs)
--___________________________________
deleteAll' _ [] = []
deleteAll' y (x:xs)
  | y == x = deleteAll' y xs
  | y /= x = [x] ++ deleteAll' y xs
--___________________________________
tambah1 x y = x + y
tambah2 x y z = x + y + z
kali1 x y = x * y
susi x y = x * y + 1
--___________________________________
-- foldl1 susi [5,1,4] = 25
--foldl1' fx (x:xs) = fx x + foldl1' fx xs
-- susi
--___________________________________
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys
--___________________________________
zipWith' fx _ [] = []
zipWith' fx [] _ = []
zipWith' fx (x:xs) (y:ys) = fx x y : zipWith' fx xs ys
--___________________________________
nth' (x:xs) 0 = x
nth' (x:xs) y = nth' xs (y-1)
--___________________________________
scanl1' fx [] = []
scanl1' fx (x:xa:xs) = [x] ++ [fx x xa] ++ scanl1' fx xs
minus x y = -x + y
-- scanl1' tambah1 [2,3,4,5,6]
-- [2,5,9,14,20]
-- [2] ++ [tambah1 x xa] ++ scanl1' tambah1 fx
-- [2] ++ [5] ++ scanl1' tambah1 [4,5]
-- [2,5] ++
--___________________________________
elem' _ [] = False
elem' y (x:xs)
  | y == x = True
  | y /= x = elem' y xs
  | otherwise = False
--___________________________________
notElem' _ [] = True
notElem' y (x:xs)
  | y == x = False
  | y /= x = notElem' y xs
  | otherwise = True
--___________________________________
head' (x:xs) = x
--___________________________________
length' [] = 0
length' (x:xs) = 1 + length' xs
--___________________________________
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = reverse' xs ++ [x]
--___________________________________
last' [x] = x
last' (x:xs) = last' (xs)
--___________________________________
tail'(x:xs) = xs
--___________________________________
init' [x] = []
init' (x:xs) = x : init' xs
--___________________________________
max' x y
  | x == y = x
  | x > y = x
  | x < y = y
--___________________________________
min' x y
  | x == y = x
  | x > y = y
  | x < y = x
--___________________________________
concat' [] = []
concat' [[]] = []
concat' [[x]] = [x]
concat' (a:as) = a ++ concat' as
--___________________________________
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' a (x:xs) = [x] ++ [a] ++ intersperse' a xs
--___________________________________
intercalate' _ [[]] = []
intercalate' [] [[x]] = [x]
intercalate' y (x:xs) = x ++ y ++ intercalate y xs
--___________________________________
and' [] = True
and' (x:xs)
  | x == False = False
  | x /= False = and' xs
--___________________________________
or' [] = False
or' (x:xs)
  | x == True = True
  | x /= True = or' xs
--___________________________________
zip3' _ _ [] = []
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' (x:xs) (y:ys) (z:zs) = [(x,y,z)] ++ zip3' xs ys zs
--___________________________________
sum' [] = 0
sum' [a] = a
sum' (x:xs) = x + sum' xs
--___________________________________
product' [] = 1
product' (x:xs) = x * product' xs
--___________________________________
--words' [] = []
--words' ['x'] = [['x']]
--words' (x:xs)
--[x] /= [' '] = [[x] ++ words' xs]
-- words' "ab ba"
-- "a" ++ words' "b ba"
-- "a" ++ "b" ++ words' " ba"
-- "ab" ++ words' " ba"
-- "ab" ++ [] ++ words' "ba"
-- "ab" ++ [] ++ "b" ++ words' "a"
--___________________________________
--lines' "" = []
--lines' ['x'] = [['x']]
--lines' (x:xs) = x : lines' xs
--lines' ['x'] = [['x']] yang ini juga kayaknya ditaro di guard aja
--lines' "\n" = [""] yang ini ditaro di guard aja
--x /= '\n' = x : lines' xs
--x /= '\n' = ([x] ++ lines' xs) : []
-- lines' "asss"
-- 'a' : lines' "sss"
-- 'a' : 's' : lines' "ss"
-- 'a' : 's' : 's' : lines' "s"
-- 'a' : 's' : 's' : 's' : lines' ""
-- 'a' : 's' : 's' : 's' : [] = "asss"
-- "a" ++ lines' "sss"
-- "a" ++ "s" ++ lines' "ss"
-- "as" ++ "s" ++ lines' "s"
-- "ass" ++ lines' "s"
-- "ass" ++ ["s"]
-- "asss" ++ []
-- "asss" ++
--[x] == ['\n'] = [x] : [] ++ lines' xs
--lines "as\nv\nc\ns\n"
-- 'a' : [] ++ lines "s\nv\nc\ns\n"
-- "a" ++ lines "s\nv\nc\ns\n"
-- "a" ++ 's' : [] ++ lines "\nv\nc\ns\n"
-- "a" ++ "s" ++ lines "\nv\nc\ns\n"
-- "as" ++ lines '\n' ++ lines' lines "v\nc\ns\n"


--x /= '\n' = (((x : []) : []) ++ lines' xs)
--x == '\n' = (x : []) ++ lines' xs) :
--lines "as\nv\nc\ns\n"
-- 'a' : [] ++ lines' "s\nv\nc\ns\n"
-- "a" ++ lines' "s\nv\nc\ns\n"
-- "a" ++ 's' : [] ++ lines' "\nv\nc\ns\n"
-- "a" ++ "s" ++ lines' "\nv\nc\ns\n"
-- "as" ++ lines' "\nv\nc\ns\n"
-- "as" ++
-- 'a' : [] ++ lines' "\nv\nc\ns\n"
-- "a" ++ lines' "\nv\nc\ns\n"
-- "a" ++ x == '\n'
-- "a"
-- "a" : [] ++ lines' "v\nc\ns\n"
-- ["a"] ++ lines' "v\nc\ns\n"
-- ["a"] ++ ('v' : []) ++ lines
-- "a" ++
-- ["a","v","c","s"]
-- lines' "cs\n"
-- 'c' : [] ++ lines' "s\n"
-- "c" ++ 's' : [] ++ lines' "\n"
-- "c" ++ "s" ++ lines' "\n"
-- "cs" ++ lines' "\n"
-- "cs" : [] ++ lines
-- ''
-- lines' (x:xs) = [(x:xs)]
--___________________________________
unlines' [] = ""
unlines' [""] = ['\n']
unlines' [[' ']] = [' '] ++ ['\n']
unlines' [['x']] = ['x'] ++ ['\n']
unlines' (x:xs) = x ++ ['\n'] ++ unlines' xs
--___________________________________
unwords' [] = ""
unwords' [""] = ""
unwords' [[' ']] = [' ']
unwords' [['x']] = ['x']
unwords' (x:xs) = x ++ [' '] ++ unwords' xs
--___________________________________
takeWhile' fx [] = []
takeWhile' fx (x:xs)
  | fx x == False = []
  | fx x == True = [x] ++ takeWhile' fx xs
--___________________________________
dropWhile' fx [] = []
dropWhile' fx (x:xs)
  | fx x == True = xs
  | fx x == False = [x] ++ xs
--___________________________________
concatMap' fx [] = []
concatMap' fx (x:xs) = fx x ++ concatMap' fx xs
--___________________________________
all' fx [] = True
all' fx (x:xs)
  | fx x == False = False
  | fx x == True = all' fx xs
--___________________________________
any' fx [] = False
any' fx (x:xs)
  | fx x == True = True
  | fx x == False = any' fx xs
--___________________________________
insert' y [] = [y]
insert' y (x:xs)
  | y < x = [y] ++ [x] ++ xs
  | y > x = [x] ++ insert' y xs
  | y == x = [y] ++ insert' y xs
--___________________________________
zipWith3' fx _ _ [] = []
zipWith3' fx _ [] _ = []
zipWith3' fx [] _ _ = []
zipWith3' fx (x:xs) (y:ys) (z:zs) = [fx x y z] ++ zipWith3' fx xs ys zs
--___________________________________
-- [4,9,5,11,4,4,3,9]
-- 4 [9]
-- deleteAll' x (x:xs) ++ deleteAll' xs xs
-- deleteAll' 4 [4,9,5,11,4,4,3,9]
-- [9,5,11,3,9] ++ deleteAll 9 [9,5,11,4,4,3,9]

--union' [] [] = []
--union' [x] [] = [x]
--union' [] [x] = [x]
--union' _ (y:ys) = union' _ ([y] ++ deleteAll' y ys)
-- union' [2,3] [2,2,4,2,2,1]
-- union' [2,3] [2] ++ deleteAll' 2 [2,4,2,2,1]
-- union' [2,3] [2] ++ [4,1]
-- union' [2,3] [2,4,1]
union' (x:xs) (y:ys)
  | x == y = [x] ++ union' xs ys
  | x /= y = [x] ++ xs ++ [y] ++ ys
--union' _ (y:ys) = union' _ ([y] ++ (deleteAll' y ys))
-- union' _ [1,2,5,8,2,1]
-- [1] ++ deleteAll' 1 [2,5,8,2,1]
-- [1] ++ [2,5,8,2]
--
--union' (x:xs)
--[y] /= ys = union' _ [y] ++ ys
-- union' _ [1,6,3,1,1]
-- union' _ [1] ++ [6] ++ union'
--y /= ys = [y]
--y == ys = [y] ++
--y == ys union' _ ([x] ++ deleteAll' x xs)
--[y] == ys = union' [x] ++ xs [y] ++ deleteAll' y ys
-- union' [1,7,8,3] [1,4,5,2]
-- [1] ++ union' [7,8,3] [4,5,2]
-- x == ys = [x] ++ union' xs ys ++ [y]
-- union' [3,4] [3,3,10,7]
-- [3] ++ union' [4] [10,7]
-- [3] ++ [4] ++ [10] ++ union' [] [7]
-- [3,4,10] ++ union' [7]
-- [3,4,10] ++ [7]
-- [3,4,10,7]
-- union' [1] [4,1,1,0,8,9,4]
-- union' _ deleteAll' 4 [1,1,0,8,9,4]
-- union' _ ([x] ++ deleteAll' x xs)
-- [1,4,0,8,9]

--x /= y = [x] ++ union' xs ys ++ [y]

-- union [6,10,8,2] [1,10,4,2,9]
-- [6,10,8,2,1,4,9]
--[x] ++ xs ++
--intersect' [] [] = []
--intersect' [_] [] = []
--intersect' [] [_] = []
--intersect' (x:xs) (y:ys)
--x == y = [x] ++ intersect'
--___________________________________
replicate' 0 _ = []
replicate' y x = [x] ++ replicate' (y-1) x
--___________________________________
--maximum' [x] = [x]
--maximum' (x:xs)
-- maximum' [7,1,3,12,0,4,3,8]
kuadrat x = [x*x]
