main :: IO()
main = do
 
 print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Bulgaria"

highestCapital :: [Country] -> String
highestCapital cs = fst $ foldl1 (\acc@(c1, h1) x@(c2, h2) -> if h1 > h2 then acc else x) $ countryAndCapitalHeight cs
 where
    countryAndCapitalHeight = map (\(Country country capital ss) -> (country, height capital ss)) 
    height capital ss = head $ [elevation | (City name elevation _) <- ss, name == capital]

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int
data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]