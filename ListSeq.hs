module ListSeq where

import Seq
import Par

data List a = Nil | Cons a (List a)

emptyL		:: List a
emptyL = Nil

singletonL	:: a -> List a
singletonL x = Cons x Nil

lengthL		:: List a -> Int
lengthL Nil			= 0
lengthL (Cons x xs) = lengthL xs + 1

nthL		:: List a -> Int -> a
nthL Nil _			= error "Invalid index."
nthL (Cons x xs) 0	= x
nthL (Cons x xs) n 	= nthL xs (n - 1)

tabulateL	:: (Int -> a) -> Int -> List a
tabulateL f n 	| n <= 0 	= error "Invalid index."
				| otherwise = tabulateL' f 0 (n - 1)
					where tabulateL' f a b 	| a == b 	= Cons (f a) Nil
											| otherwise = let (x, xs) = f a ||| aux f (a + 1) b in Cons x xs

mapL		:: (a -> b) -> List a -> List b
mapL _ Nil 			= Nil
mapL f (Cons x xs)	= let (x', xs') = f x ||| mapL f xs
						in Cons x' xs'

filterL		:: (a -> Bool) -> List a -> List a
filterL _ Nil 			= Nil
filterL f (Cons x xs)	= let (p, xs') = f x ||| filterL f xs
							in if p then Cons x xs' else xs'

appendL		:: List a -> List a -> List a
appendL Nil ys 			= ys
appendL (Cons x xs) ys 	= Cons x (appendL xs ys)

takeL		:: List a -> Int -> List a
takeL xs 0 			= Nil
takeL Nil n 		= error "Invalid index."
takeL (Cons x xs) n = Cons x (takeL xs (n - 1))

dropL		:: List a -> Int -> List a
dropL xs 0 			= xs
dropL Nil n			= error "Invalid index."
dropL (Cons x xs) n = dropL xs (n - 1)


showtL		:: List a -> TreeView a (List a)
showtL Nil 			= EMPTY
showtL (Cons x Nil)	= ELT x
showtL xs 			= let (l', r') = takeL xs (quot l 2) ||| dropL xs (quot l 2) in NODE l' r'
						where l = lengthL xs

showlL     	:: List a -> ListView a (List a)
showlL Nil 			= NIL
showlL (Cons x xs)	= CONS x xs

joinL		:: List (List a) -> List a
joinL Nil 			= Nil
joinL (Cons x xs) 	= appendL x (joinL xs)

reduceL		:: (a -> a -> a) -> a -> List a -> a
reduceL f b Nil 			= b
reduceL f b (Cons x Nil) 	= f b x
reduceL f b (Cons x xs) 	= reduceL f e (combine f xs)
								where	combine _ Nil 					= Nil
										combine _ (Cons x Nil) 			= (Cons x Nil)
										combine f (Cons x (Cons y ys))	= let (x', xs') = f x y ||| combine f ys in Cons x' xs'

scanL		:: (a -> a -> a) -> a -> List a -> (List a, a)
scanL f b Nil 			= (Nil, b)
scanL f b (Cons x Nil)	= (Cons b Nill, f b x)
scanL f b (Cons x xs) 	= error "Not implemented."

fromListL	:: [a] -> List a
fromListL []		= Nil
fromListL (x:xs) 	= Cons x (fromListL xs)

instance Seq List where
	emptyS		= emptyL
	singletonS	= singletonL
	lengthS		= lengthL
	nthS		= nthL
	tabulateS	= tabulateL
	mapS		= mapL
	filterS		= filterL
	appendS		= appendL
	takeS		= takeL
	dropS		= dropL
	showtS		= showtL
	showlS		= showlL
	joinS		= joinL
	reduceS		= reduceL
	scanS		= scanL
	fromList	= fromListL

instance Show a => Show (List a) where
	show p 	= "<" ++ show' p (lengthL p) 0
			where show' p n i 	| i == n     	= ">"
								| i == (n - 1) 	= show (nthL p i) ++ ">"
								| otherwise 	= show (nthL p i) ++ "," ++ show' p n (i + 1)
