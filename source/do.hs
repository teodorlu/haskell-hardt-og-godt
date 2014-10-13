import Types
import Control.Monad (guard, MonadPlus, mzero, mplus)

main = do
	name <- getLine
	putStrLn $ "Hello, " ++ name ++ "!"

grandfathers'' p = do
	ff <- father p >>= father
	mf <- mother p >>= father
	return (ff, mf)

grandfathersFail p = do
	ff <- father p >>= father
	mf <- mother p >>= father
	_ <- Tull
	return (ff, mf)

pythagoreans = do
	z <- [1..]
	y <- [1..z]
	x <- [1..y]
	guard (x*x + y*y == z*z)
	return (x, y, z)

pythagoreans' = do
	z <- [1..]
	y <- [1..z]
	x <- [1..y]
	_ <- filter (\w -> x*x + y*y == z*z) [1]
	return (x, y, z)

instance MonadPlus Kanskje where
	mzero = Tull
	mplus (Bare x) _  = Bare x
	mplus _        kx = kx

grandfathersNotHitler p = do
	ff <- father p >>= father
	mf <- mother p >>= father
	guard (name ff /= "Hitler")
	return (ff, mf)
