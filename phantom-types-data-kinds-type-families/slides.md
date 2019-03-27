---
title: Introduction to type level magic in Haskell
author: Jan Šipr <br/>[github.com/siprj](https://github.com/siprj)

// TODO: date ....
date: 
---


## Agenda

[//]: # (Do I need phantom types???)
* Phantom types
* Data Kinds
* Multi-parameter type classes
* Functional Dependences
* Type Families
* Short live coding session


## What is a Phantom type?

A phantom type is a parametrised type whose parameters do not all appear on
the right-hand side of its definition.


## Phantom type examples

~~~ { .haskell }
data Foo a = Foo Int
data Foo a b = Foo Int b
~~~


## This is not the Phantom type!!!

~~~ { .haskell }
type Foo a = String
~~~

::: notes

* Type alias is expanded before type check.

:::


## Why should I care?

~~~ { .haskell }
data Id a = Id Word64
~~~

~~~ { .haskell }
data Distance a = Distance Word64
~~~

~~~ { .haskell }
data Proxy a = Proxy
~~~


## Kinds

* In type theory kind is type of a type.
* Exists only at compile time.


~~~ { .haskell }
Int :: *
Maybe :: * -> *
Maybe Bool :: *
a -> a :: *
[] :: * -> *
(->) :: * -> * -> *
~~~


## GHC Extensions extending kinds

* ConstraintKinds
* DataKinds
* KindSignatures
* PolyKinds


## Data kinds

* The **_DataKinds_** extension allows us to promote **_data
constructors_** into **_type constructors_**, which also promotes their
**_type constructors_** into **_kind constructors_**.

* Giving Haskell a Promotion


## Type constructor vs. Data constructor

~~~ { .haskell }
data Foo a = Bar | Baz
~~~

* `Foo a` is **type** constructor
* `Bar`, `Baz` are **data** constructors


## Data kinds literals

In `GHC.TypeLits` you can find:

* `Nat`
* `Symbol`
* Conversion functions
* Type classes
* And many others


## Data kinds literals examples

~~~ { .haskell }
foo :: Maybe "bar"
foo2 :: Maybe 10
~~~

~~~ { .haskell }
Maybe ("bar" :: Symbol)
Maybe (10 :: Nat)
~~~

::: notes

* Reference to easytensor

:::


## Kinds and constraints

~~~ { .haskell }
class KnownSymbol (n :: Symbol)
~~~

~~~ { .haskell }
class KnownNat (n :: Nat)
~~~

::: notes

* Some times we want to force some kind on unknown type.
* Every literal like "hello" or 10 has these instance for above.

:::


## Custom data kind definition

Just like standard data structures:

~~~ { .haskell }
data Foo = Bar | Baz Foo
~~~

~~~ { .haskell }
data Foo a = Bar a
~~~


## Promotion restrictions

* We only promote datatypes whose kinds are of the form `* -> ... -> * -> *`
* We do not promote datatypes whose constructors are kind polymorphic,
  involve constraints, or use existential quantification.

::: notes

~~~ { .haskell }
data Fix f = In (f (Fix f))
~~~

:::


## Data promotion

* Quite often automatic
* But sometimes we need to use apostrophe

~~~ { .haskell }
type Foo = ["hello", "world"]
type Bar = "hello" ': "world" ': '[]
~~~


## Id returns again

~~~ { .haskell }
data IdType = UserId | CarId

data Id (a :: IdType) = Id

processUser :: Id 'UserId -> IO ()
processUser id = ...

processCar :: Id 'CarId -> IO ()
processCar id = ...
~~~


## Other Data kinds usage

* EDSLs e.g. Servant
* Type level numeric checks e.g. easytensor
* guarantee well-formedness


## Multi-parameter type classes

Requires extension `MultiParamTypeClasses`

~~~ { .haskell }
class Monad m => VarMonad m v where
  new :: a -> m (v a)
  get :: v a -> m a
  put :: v a -> a -> m ()

instance VarMonad IO IORef where ...
instance VarMonad (ST s) (STRef s) where ...
~~~


## Functional Dependences

Requires extension `MultiParamTypeClasses`





## Type families


## Questions?

* Thank you for your attention.
* Slides will be available at [github.com/siprj/presentations](https://github.com/siprj/presentations)
