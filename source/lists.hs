infixr 5 :::
data List a = Empty | a ::: (List a)
	deriving (Show, Read, Eq, Ord)

countToTen 10 = [10]
countToTen n = n : countToTen (n+1)

countToTen' n
  | n > 10    = []
  | otherwise = n : countToTen' (n+1)

map' f []     = []
map' f (x:xs) = f x : map f xs

bartender n
	| n < 18    = "Milk"
	| n < 20    = "Beer"
	| otherwise = "Whisky"

filter' cond [] = []
filter' cond (x:xs)
	| cond x    = x : filter' cond xs
	| otherwise = filter' cond xs

