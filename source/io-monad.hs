main =
	putStrLn "Please enter your name " >>= \trash ->
	getLine >>= \name ->
	putStrLn ("Hello, " ++ name ++ "!")
