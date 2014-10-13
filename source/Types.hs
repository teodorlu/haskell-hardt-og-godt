module Types where

data TrafficLightState = RedLight | YellowLight | GreenLight
  deriving (Show, Eq)

data CarSpeed = FastDriving | SlowDriving
  deriving (Show, Eq)

shouldStop :: CarSpeed -> TrafficLightState -> Bool
shouldStop    _           RedLight           = True
shouldStop    _           GreenLight         = False
shouldStop    speed       YellowLight        = speed == SlowDriving

data Vector3 = Vector3 Double Double Double
  deriving (Show, Eq)

scalarMult3 :: Double -> Vector3 -> Vector3
scalarMult3 a (Vector3 x y z) = Vector3 (a*x) (a*y) (a*z)

vector3Add :: Vector3 -> Vector3 -> Vector3
vector3Add (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1+x2) (y1+y2) (z1+z2)

class Vector a where
	vectorAdd  :: a -> a -> a
	scalarMult :: Double -> a -> a

instance Vector Vector3 where
	vectorAdd  = vector3Add
	scalarMult = scalarMult3

data Vector2 = Vector2 Double Double
  deriving (Show, Eq)

vector2Add :: Vector2 -> Vector2 -> Vector2
vector2Add (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1+x2) (y1+y2)

scalarMult2 :: Double -> Vector2 -> Vector2
scalarMult2 a (Vector2 x y) = Vector2 (a*x) (a*y)

instance Vector Vector2 where
	vectorAdd  = vector2Add
	scalarMult = scalarMult2

data Kanskje a = Tull | Bare a
	deriving (Show, Eq)

atIndex :: [a] -> Int -> Kanskje a
atIndex [] _ = Tull
atIndex (x:xs) i
	| i <  0    = Tull
	| i == 0    = Bare x
	| otherwise = atIndex xs (i-1)

data Person = Person {
	name :: String,
	father :: Kanskje Person,
	mother :: Kanskje Person
} deriving (Eq)

instance Show Person where
	show = name

leif = Person "Leif" Tull Tull
tore = Person "Tore" (Bare leif) Tull

alf = Person "Alf" Tull Tull
hanne = Person "Hanne" (Bare alf) Tull

teodor = Person "Teodor" (Bare tore) (Bare hanne)

fathersfather :: Person -> Kanskje Person
fathersfather person =
  case father person of Bare f -> father f
                        otherwise -> Tull

mothersfather :: Person -> Kanskje Person
mothersfather person =
  case mother person of Bare m -> father m
                        otherwise -> Tull

grandfathers person =
  case fathersfather person of Bare ff ->
                                 case mothersfather person of Bare mf -> Bare (ff, mf)
                                                              otherwise -> Tull
                               otherwise -> Tull

bindKanskjePerson :: Kanskje Person -> (Person -> Kanskje Person) -> Kanskje Person
bindKanskjePerson Tull     _ = Tull
bindKanskjePerson (Bare p) f = f p

bindKanskje :: Kanskje a -> (a -> Kanskje b) -> Kanskje b
bindKanskje (Bare x) f = f x
bindKanskje Tull     _ = Tull

combineKanskje :: Kanskje a -> Kanskje b -> Kanskje (a, b)
combineKanskje (Bare x) (Bare y) = Bare (x, y)
combineKanskje _        _        = Tull

fathersfather' :: Person -> Kanskje Person
fathersfather' p = (father p) `bindKanskje` father

mothersfather' :: Person -> Kanskje Person
mothersfather' p = (mother p) `bindKanskje` mother

grandfathers' p = combineKanskje (fathersfather' p) (mothersfather' p)

instance Monad Kanskje where
	(>>=) = bindKanskje
	return = Bare
