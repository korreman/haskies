type Weight = Double
type Height = Double

getNum :: IO Double
getNum = read <$> getLine

getData :: IO (Weight, Height)
getData =
  (,)
  <$> (putStr "Write your weight: " *> getNum)
  <*> (putStr "Write your height: " *> getNum)

calcBMI :: Weight -> Height -> Double
calcBMI w h = w / h ** 2

classifyBMI :: Double -> String
classifyBMI bmi | bmi < 18.5 = "This is underweight."
                | bmi < 25.0 = "This is within the normal range."
                | bmi < 30.0 = "This is overweight."
                | otherwise  = "This is obese."

main :: IO ()
main =
  getData >>= \ (w, h) ->
  let bmi = calcBMI w h in
  putStr "Your BMI is: " *> print bmi *>
  putStrLn (classifyBMI bmi)
