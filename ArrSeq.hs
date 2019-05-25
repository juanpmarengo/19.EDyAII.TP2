module ArrSeq where

import Seq
import qualified Arr as A
import Arr ((!))
import Par

emptyA	:: A.Arr a
emptyA  = A.fromList []

singletonA	:: a -> A.Arr a
singletonA x = A.fromList [x]

mapA		:: (a -> b) -> A.Arr a -> A.Arr b
mapA f xs = A.tabulate (\i -> f (xs!i)) (A.length xs)

filterA		:: (a -> Bool) -> A.Arr a -> A.Arr a
filterA f xs = A.flatten (mapA (\x -> if f x then singletonA x else emptyA) xs)

appendA		:: A.Arr a -> A.Arr a -> A.Arr a
appendA xs ys = A.flatten (A.fromList [xs, ys])

takeA		:: A.Arr a -> Int -> A.Arr a
takeA xs n = A.subArray 0 n xs

dropA		:: A.Arr a -> Int -> A.Arr a
dropA xs n = A.subArray n ((A.length xs) - n) xs

showtA		:: A.Arr a -> TreeView a (A.Arr a)
showtA xs = error "Not implemented."

showlA     	:: A.Arr a -> ListView a (A.Arr a)
showlA xs = error "Not implemented."

reduceA		:: (a -> a -> a) -> a -> A.Arr a -> a
reduceA f b xs = error "Not implemented."

scanA		:: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
scanA = error "Not Implemented."

instance Seq A.Arr where
	emptyS		= emptyA
	singletonS	= singletonA
	lengthS		= A.length
	nthS		= (!)
	tabulateS	= A.tabulate
	mapS		= mapA
	filterS		= filterA
	appendS		= appendA
	takeS		= takeA
	dropS		= dropA
	showtS		= showtA
	showlS		= showlA
	joinS		= A.flatten
	reduceS		= reduceA
	scanS		= scanA
	fromList	= A.fromList

xs = A.fromList [1, 3, -2, 5]