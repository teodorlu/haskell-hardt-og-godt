data TrafficLightState = RedLight | YellowLight | GreenLight
  deriving (Show, Eq)

data CarSpeed = FastDriving | SlowDriving
  deriving (Show, Eq)

shouldStop :: CarSpeed -> TrafficLightState -> Bool
shouldStop    _           RedLight           = True
shouldStop    _           GreenLight         = False
shouldStop    speed       YellowLight        = speed == SlowDriving

data Vector3d =
	  IntVector Int Int Int
	| DoubleVector Double Double Double
	deriving (Show, Eq)

data PersonalValue =
	KnowsProgrammingLanguages Int | 
	HasMotto String |
	HasTits
	deriving (Show, Eq)

data PersonInfo = PersonInfo {
	name :: String,
	age :: Int
}
	deriving (Show, Eq)
