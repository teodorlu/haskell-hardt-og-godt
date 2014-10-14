addOne :: Int -> Int
addOne x = x + 1

add = (+)

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER 7!"
lucky _ = "Too bad :("

applyToBothThenAdd f x y = add (f x) (f y)

isSpeaker :: String -> Bool
isSpeaker "Teodor" = True
isSpeaker _        = False

squaredSum :: Int -> Int -> Int
squaredSum x y = x*x + y*y
