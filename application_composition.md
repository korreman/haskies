# Function application and composition

This doc is about the operators `($)`, `(&)`, `(.)`, and `(>>>)`.

Application - `($)`
-----
`($)` is the function application operator. It is defined as follows:
```
($) :: (a -> b) -> a -> b
f $ x = f x
```
This may seem completely redundant. Function application is the most basic
operation! But it has some uses.

First, it can be practical in many cases to pass *function application itself*
to other functions.

Secondly, `($)` has a precedence of `infixr 0`. This means that everything else
will be computed first and allows `($)` to be used as a substitute for
parentheses. For example, something like
```
print ("You have " ++ show msgNum ++ " new messages!")
```
Can be written as:
```
print $ "You have " ++ show msgNum ++ " new messages!"
```
We can do this for many nested parentheses, so `f (g (h x))` becomes `f $ g $ h
x` (if `f`, `g`, and `h` are functions).

Reverse application - `(&)`
------------
Can be imported with `import Data.Function ((&))`.

This operator also does function application, but reverses the order of the
arguments.
```
(&) :: a -> (a -> b) -> b
x & f = f x
```
This is known in some languages as *piping*. In bash, it is done with the
operator `|`. In F#, it is done with `|>`. It's a pretty intuitive way of
visualizing the flow of data from left to right. For example, we can rewrite:
```
primeSquareSum xs = sum (map (^2) (filter isPrime xs))
```
To the more readable:
```
primeSquareSum xs = xs & filter isPrime & map (^2) & sum
```

(WIP) Composition - `(.)`
-------------------
Function composition looks confusingly similar to function application, so let's
use an analogy.

If we imagine a function to be a pipe, then:
* Function application is to put a value into the pipe and get out a new one on
  the other end.
* Function composition is to take two pipes and stick them together at the ends.
  No values are involved.

```
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \ x -> f (g x)
```
Given a function:
```
someFunc x = f (g (h x))
```
We can rewrite it with composition as:
```
someFunc = f . g . h
```
Notice that no arguments are given.

Reverse composition - `(>>>)`
-----------------------------
Can be used with `import Control.Arrow ((>>>))`.

Similarly to `(&)`, this is just composition with the arguments reversed.
```
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
g >>> f = \ x -> f (g h)
```
It's a bit more intuitive to understand, as we naturally read in a left-to-right
fashion. If you had a hard time understanding `(.)`, try to look at the
explanations of that, but with the definition for `(>>>)`.
