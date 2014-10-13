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
vector3Length (Vector2 x y) = sqrt (x*x + y*y)

scalarMult2 :: Double -> Vector2 -> Vector2
scalarMult2 a (Vector2 x y) = Vector2 (a*x) (a*y)

instance Vector Vector2 where
	vectorLength = vector2Length
	scalarMult   = scalarMult2

data PersonInfo = PersonInfo {
	name :: String,
	age :: Int
}
	deriving (Show, Eq)

