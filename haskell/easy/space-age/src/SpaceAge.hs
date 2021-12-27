module SpaceAge (Planet (..), ageOn) where

type Seconds = Float
type Years = Float

data Planet
  = Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune

-- | Calculate how many years old someone would be on a Planet, given an age in seconds.
ageOn :: Planet -> Seconds -> Years
ageOn planet seconds = seconds / orbitalPeriod planet

-- | The orbital period of Earth, in seconds.
earthYear :: Seconds
earthYear = 31557600

-- | The orbital period of a planet, in seconds.
orbitalPeriod :: Planet -> Seconds
orbitalPeriod Mercury = 0.2408467 * earthYear
orbitalPeriod Venus = 0.61519726 * earthYear
orbitalPeriod Earth = 1.0 * earthYear
orbitalPeriod Mars = 1.8808158 * earthYear
orbitalPeriod Jupiter = 11.862615 * earthYear
orbitalPeriod Saturn = 29.447498 * earthYear
orbitalPeriod Uranus = 84.016846 * earthYear
orbitalPeriod Neptune = 164.79132 * earthYear
