# Contexts

We will often be using the term 'context' going forward. A context is a type
that takes one or more other types. Some examples are `Maybe`, `[]`, and
`Either`. All of these need another type to be complete. We will usually write
them as `Maybe a`, `[a]`, and `Either a b`, where `a` and `b` are generic types.

Some examples of complete types are `Maybe Int`, `[String]`, and `Either String
Int`. We say that `Maybe` *is a context for* `Int`, and that `Int` *is a
subtype* in `Maybe Int`.

Why not just say containers?
----------------------------

One might be tempted to think that contexts are synonymous with containers, but
this is misleading. The reason is that many contexts are just functions. For
example `Reader a b` contains a function `a -> b`. It contains neither values of
type `a` or `b`. Rather, it is a context for `a` and `b`, in that `a` and `b`
are a parameter and return value respectively.

Usually just one sub-type
---------------------------

Generally, we'll use contexts that have just one subtype. `Maybe a`, `[a]`,
`Vector a`, `Parser a`.

Furthermore, partially complete types are still treated as contexts. For example
`Either Int a` and `Reader String a` are both contexts with one single subtype.

Examples
--------

Contexts and their descriptions:
* `Maybe a` - A value of the type `a` may or may not exist.
* `[a]` - A linked list of values with type `a`.
* `Either a b` - Either a value of type `a` or a value of type `b`.
* `Reader String a` - A function that transforms an `Int` into an `a`.
* `State s a` - A function that modifies a state `s` while also returning a
  value `a`.

Type constructors
-----------------

This is really just a synonym for type constructors, but it rolls better off the
tongue. It also provides a linguistic basis for talking more accurately about
type constructors.
