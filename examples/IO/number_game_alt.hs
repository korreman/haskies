import Control.Applicative (Alternative, empty)
import Data.Functor ((<&>))
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import System.Random (randomRIO)

-- Applies the predicate to inner values.
-- If the result is true, does nothing.
-- If the result is false, replaces with empty.
--
-- For lists, this is equivalent to filter.
-- For @Maybe@, this will replace the value with @Nothing@ if the predicate
-- doesn't hold.
validate :: (Alternative t, Monad t) => (a -> Bool) -> t a -> t a
validate p x = x >>= \v -> if p v then pure v else empty

-- Retrieves a valid guess
getGuess :: IO Int
getGuess =
  putStr "Type your guess: " *>
  (readMaybe <$> getLine) <&>
  validate (\ x -> 1 <= x && x <= 100) >>=
  maybe (putStrLn "Come again?" *> getGuess) pure

numberGame :: Int -> Int -> IO ()
numberGame 0 num = putStrLn ("You lose! The number was " ++ show num ++ ".")
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
