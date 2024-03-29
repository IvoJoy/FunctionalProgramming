main :: IO()
main = do

    print $ getVolumes [(5, 10), (5, 2), (2, 10), (2, 5)] == [785.4, 157.08, 125.66, 62.83]

type Cylinder = (Double, Double)

roundD :: Double -> Double
roundD n = fromIntegral (round (n * 100)) / 100

getVolumes :: [Cylinder] -> [Double]
getVolumes xs =  [roundD (pi * r * r * h ) | (r, h) <- xs]