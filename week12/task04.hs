import Data.List
main :: IO()
main = do
 
 print $ closestAverage [(Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)]

closestAverage :: [Measuring] -> Int
closestAverage temps = day $ foldl1 (\ t1@(Temp _ temp1) t2@(Temp _ temp2) -> if abs (average - temp1) < abs (average - temp2) then t1 else t2) temps  
 where
     average = sum [temp | (Temp _ temp) <- temps] / fromIntegral (length temps)
     day (Temp d _) = d

data Measuring = Temp Int Float