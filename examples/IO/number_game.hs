import System.Random
import Text.Read (readMaybe)

-- Retrieves a valid guess
getGuess :: IO Int
getGuess =
  putStr "Type your guess: " *>
  (readMaybe <$> getLine) >>= \ num ->
  case num of
    Just num | num >= 1 || num <= 100 -> pure num
    Nothing -> putStrLn "Come again?" *> getGuess
    _ -> putStrLn "Number needs to be from 1 to 100" *> getGuess

numberGame :: Int -> Int -> IO ()
numberGame 0 _ = putStrLn ("You lose! The number was " ++ show num ++ ".")
numberGame guesses num =
  putStrLn ("You have " ++ show guesses ++ " guesses left.") *>
  getGuess >>= \ guess ->
  case compare guess num of
    EQ -> putStrLn "Correct. You win!\n"
    LT -> putStrLn "Too low. Try again!\n"  *> numberGame (guesses - 1) num
    GT -> putStrLn "Too high. Try again!\n" *> numberGame (guesses - 1) num

main :: IO ()
main =
  putStrLn "I'm thinking of a number from 1 to 100. You have 7 guesses." *>
  (randomRIO (1, 100) >>= numberGame 7) *>
  putStrLn "Do you want to play again? (y/n)" *>
  getLine >>= \line -> case line of
    "y" -> putStr "\n" *> main
    "n" -> putStrLn "Goodbye."
    _ -> putStrLn "I'll take that as a yes!\n" *> main
