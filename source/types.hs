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

vector3Length :: Vector3 -> Double
vector3Length (Vector3 x y z) = sqrt (x*x + y*y + z*z)

class Vector a where
	vectorLength :: a -> Double
	scalarMult   :: Double -> a -> a

instance Vector Vector3 where
	vectorLength = vector3Length
	scalarMult   = scalarMult3

data Vector2 = Vector2 Double Double
  deriving (Show, Eq)

vector2Length :: Vector2 -> Double
vector2Length (Vector2 x y) = sqrt (x*x + y*y)

scalarMult2 :: Double -> Vector2 -> Vector2
scalarMult2 a (Vector2 x y) = Vector2 (a*x) (a*y)

instance Vector Vector2 where
	vectorLength = vector2Length
	scalarMult   = scalarMult2

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
} deriving (Show, Eq)

leif = Person "Leif" Tull Tull
tore = Person "Tore" (Bare leif) Tull

alf = Person "Alf" Tull Tull
hanne = Person "Hanne" (Bare alf) Tull

teodor = Person "Teodor" (Bare tore) (Bare hanne)

farfar :: Person -> Kanskje Person
farfar person =
  case father person of Bare f -> father f
                        otherwise -> Tull

morfar :: Person -> Kanskje Person
morfar person =
  case mother person of Bare m -> father m
                        otherwise -> Tull

bestefedre person =
  case farfar person of Bare ff -> case morfar person of Bare mf -> Bare (ff, mf)
                                                         otherwise -> Tull
                        otherwise -> Tull
