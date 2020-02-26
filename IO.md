I/O in Haskell
--------------
*You are encouraged to run these code snippets and experiment with them!*

Input and output in Haskell is done with the `IO a` type.

The type `IO a` represents an IO action that results in a value of type `a`.
IO actions can be composed into compound IO actions. A value can never be
unwrapped from IO. A well-formed Haskell program is just one big compound IO
action: `main :: IO ()`.

Doing nothing
-------------
Before we dive in, let's take a look at `pure` first:
```
pure :: a -> IO a
```
This will simply wrap a value in the IO context. The IO action does nothing and
results in whatever value was given. Thus we can construct a dummy IO action:
```
pure () :: IO ()
```
This does nothing and results in nothing. We can use it to make a dummy program:
```
main :: IO ()
main = pure ()
```

Output
------
Let's start with the function:
```
putStr :: String -> IO ()
putStrLn :: String -> IO ()
```
`putStr` takes string and returns an IO action that prints said string to
standard out. `putStrLn` does the same, but adds a newline. Using this we can
make our Hello World:
```
main :: IO ()
main = putStr "Hello, World!\n"
```

We can also use:
```
show :: Show a => a -> String
print :: Show a => a -> IO ()
```
`show` converts a value to a string, as long as that value is a member of the
`Show` typeclass.

`print` will convert a value to a string and then print it. It is equivalent to
```
print x = putStrLn (show x)
```

With this we can print numbers and other arbitrary values:
```
main = print (13, 17)
```

If we want to combine multiple IO actions, we can do so with various operators.
These operators combine the actions, resulting in new IO actions that can be
combined once again. To combine outputs, the operators `(<*)` and `(*>)` will
suffice:
```
(*>) :: IO () -> IO () -> IO ()
(<*) :: IO () -> IO () -> IO ()
```
These combine two actions, performing them from left to right. Note that the
direction of the arrow *does not* affect the order of actions. The arrow points
to the result to keep, throwing the other result away. Pure output actions
always result in `()`, so you can use either arrow. An example:
```
name   = "Xavier"
age    = 27
height = 1.82

main =
  putStr "Name: " *> putStr name *> putStr "\n" *>
  putStr "Age: " *> print age *>
  putStr "Height: " *> print height
```
Try flipping all the arrows so they point left. When running the new program,
you will see that nothing has changed.

Input
-----
Actions can also read values. However, *you cannot extract values from the IO
context, ever*. For example:
```
getLine :: IO String
```
This will read one line from standard input and return it. You cannot simply
convert it to a `String`. So what do we do instead?

The answer is that we apply functions to the value inside with `(<$>)`:
```
(<$>) :: (a -> b) -> IO a -> IO b
```
Example:
```
-- `read` converts a string to any type that is a member of the `Read` class
getHeight :: IO Float
getHeight = read <$> getLine

getHeightInches :: IO Float
getHeightInches = (\ x -> x * 100 / 2.54) <$> getHeight
```

Second, we can apply functions to multiple values by using `(<$>)` along with:
```
(<*>) :: IO (a -> b) -> IO a -> IO b
```
Example:
```
getNum :: IO Int
getNum = read <$> getLine

addNum :: IO Int
addNum = (\ x y z -> x + y + z) <$> getNum <*> getNum <*> getNum
```
It's really important to note that each use of `getNum` is distinct. The above
action will read a number, read a second number, read a third, and then add the
three numbers together.

Third, the final piece of the puzzle: taking the result of an IO input action and
using it for an IO output action. Say we want to read a line and then print it
again. We can get halfway there by using `(<$>)`:
```
main :: IO ()
main = putStrLn <$> getLine
```
This won't compile. The problem is that our type is now `IO (IO ())` instead of
`IO ()`. To "flatten" the type again, we use:
```
join :: IO (IO a) -> IO a
```
This will perform the outer action, then perform the resulting inner action. We
can then write our program as:
```
main = join (putStrLn <$> getLine)
```
As `(<$>)` and `join` are almost always used in conjunction, there's a nifty
operator for this:
```
(>>=) :: IO a -> (a -> IO b) -> IO b
x >>= f = join (f <$> x)
```
Our program written with the new operator:
```
main = getLine >>= putStrLn
```

Putting it together
-------------------
Let's create a BMI calculator. It retrieves your height and weight. It then
calculates your BMI and tells you whether this is in a safe range.

First, getting the data. We use the `getNum` from earlier to read numbers:
```
type Weight = Double
type Height = Double

getNum :: IO Double
getNum = read <$> getLine

getData :: IO (Weight, Height)
getData =
  (,)
  <$> (putStr "Write your weight: " *> getNum)
  <*> (putStr "Write your height: " *> getNum)
```
Next, we create a function to calculate BMI, and a function which classifies
this BMI.
```
calcBMI :: Weight -> Height -> Double
calcBMI w h = w / h ** 2

classifyBMI :: Double -> String
classifyBMI bmi | bmi < 18.5 = "This is underweight."
                | bmi < 25.0 = "This is within the normal range."
                | bmi < 30.0 = "This is overweight."
                | otherwise  = "This is obese."

```
We create an IO function to print a BMI and its classification
```
printResults :: Double -> IO ()
printResults bmi =
  putStr "Your BMI is: " *> print bmi *>
  putStrLn (classifyBMI bmi)
```

Finally, we stitch these together:
```
main :: IO ()
main = uncurry calcBMI <$> getData >>= printResults
```

Do-notation
-----------
Haskell has a special syntax to make things like IO easier. It is called
do-notation. The syntax is relatively simple:
```
main =                    main =
  do expr1                  do { expr1
     expr2       or            ; expr2
     expr3                     ; expr3 }
```
Lines can take three forms. It is shown best with an example:
```
do putStrLn "What is your name?"
   name <- getLine
   let reverseName = reverse name
   putStrLn ("Hello, " ++ reverseName ++ "!")
```
This does the following:
  1. Print the question.
  2. Run the `getNum` action and bind the result to `name`.
  3. Apply `reverse` to `name`, binding the result to `reverseName`.
  4. Print the reversed name in a sentence.

This is directly equivalent to:
```
putStrLn "What is your name?" *>
getLine >>= \ name ->
let reverseName = reverse name in
putStrLn ("Hello, " ++ reverseName ++ "!")
```

We can rewrite the IO part of our BMI calculator using the new syntax:
```
main =
  do
    putStr "Write your weight: "
    weightTxt <- getLine
    putStr "Write your height: "
    heightTxt <- getLine

    let weight = read weightTxt :: Weight
    let height = read heightTxt :: Height
    let bmi = calcBMI weight height

    putStr "Your BMI is: "
    print (calcBMI weight height)
    putStrLn (classifyBMI bmi)
```

Notes
------------
These actions all handled standard input and output in the terminal, but IO can
also read from and write to files, as well as communicate with other processes.

You can use `(<$>)` because IO is an instance of the `Functor` typeclass.
`(<$>)` is an inline synonym for `fmap`.

You can use `(*>)`, `(<*)`, `pure`, and `<*>` because IO is an instance of the
`Applicative` typeclass. These are the same as the now-redundant `(>>)`, `(<<)`,
`return`, and `ap` for monads.

You can use `join` and `(>>=)` because IO is an instance of the `Monad`
typeclass. There are also other operators: `(=<<)`, `(>=>)`, `(<=<)`. These are
worth checking out.
