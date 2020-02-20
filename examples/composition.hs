import Control.Arrow ((>>>))

main = sumlines <$> readFile "input_day1.txt" >>= putStrLn

sumlines :: String -> String
sumlines =
    filter (/= '+') >>>
    lines >>>
    map (read :: String -> Int) >>>
    sum >>>
    show
