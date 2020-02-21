# Common IO functions

* `interact :: (String -> String) -> IO ()`

  Applies the function to the input from `stdin` and prints it to `stdout`.

* `putChar :: Char -> IO ()`

  Prints a character to `stdout`.

* `putStr :: String -> IO ()`

  Prints a string to `stdout`.

* `putStrLn :: String -> IO ()`

  Same as `putStr`, but prints a newline as well.

* `print :: Show a => a -> IO ()`

  Prints a value to `stdout`. The value must be a member of `Show`, so that it
  can be converted to a string.

* `getChar :: IO Char`

  Reads a single character from `stdin`.

* `getLine :: IO String`

  Reads a single line from `stdin`.

* `getContents :: IO String`

  Reads everything from `stdin`.

* `readFile :: String -> IO String`

  Reads the contents of the file with the given path.

* `writeFile :: String -> String -> IO ()`

  Writes a string to a file. The first argument is the file path, the second is
  the content to write to it.

* `appendFile :: String -> String -> IO ()`

  Appends a string to the end of a file. The first argument is the file path,
  the second is the content to append to.
