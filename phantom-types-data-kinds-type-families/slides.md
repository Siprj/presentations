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


## How does phantom type look like?

``` { .haskell }
data Foo a = Foo Int
```

``` { .haskell }
data Foo a b = Foo Int b
```


## This is not the Phantom type!!!

``` { .haskell }
type Foo a = String
```

::: notes

* Type aliases are expanded before type check.

:::


## What can I do with phantom types? 1/3

``` { .haskell }
data Id a = Id Word64

type UserId = Id UserData

type = Map UserId UserData
```

## What can I do with phantom types? 2/3

``` { .haskell }
data Distance a = Distance Word64

addDistances :: Distance a -> Distance a
```

## What can I do with phantom types? 3/3

``` { .haskell }
data Proxy a = Proxy

serve :: HasServer api '[] => Proxy api -> Server api -> Application
```


## Before kinds

``` { .haskell }
data Foo a = Bar | Baz
```

* `Foo a` is **type** constructor
* `Bar`, `Baz` are **data** constructors

::: notes

Before `data kinds`.

Explain what is `data constructor` and what is `type constructor`.

:::


## Kinds

* In type theory kind is type of a type.
* Exists only at compile time.


``` { .haskell }
Int :: *
Maybe a :: * -> *
Maybe Bool :: *
[a] :: * -> *
(->) :: * -> * -> *
```

::: notes

* Explain what the star is and how you can use GHC to show it.
* By default kinds are in GHC we just can't express it. That is where
  `KindSignatures` comes in.

:::


## GHC Extensions extending kinds

* KindSignatures
* ConstraintKinds
* DataKinds
* PolyKinds


## Kind signatures

``` { .haskell }
class Foo (a :: * -> *) where
```

``` { .haskell }
class Bar (n :: *) where
```

``` { .haskell }
data Baz (a :: * -> *) = Foo Int (a String)
```


## Data kinds

* The **_DataKinds_** extension allows us to promote **_data
constructors_** into **_type constructors_**, which also promotes their
**_type constructors_** into **_kind constructors_**.

* Based on paper Giving Haskell a Promotion


## Custom data kind definition

Just like standard data structures:

``` { .haskell }
data Foo = Bar | Baz Foo
```

``` { .haskell }
data Foo a = Bar a
```


## Data promotion

* Quite often automatic
* But sometimes we need to use apostrophe

``` { .haskell }
type Foo = ["hello", "world"]
type Bar = "hello" ': "world" ': '[]
```


## Promotion restrictions

* We only promote datatypes whose kinds are of the form `* -> ... -> * -> *`
* We do not promote datatypes whose constructors are kind polymorphic,
  involve constraints, or use existential quantification.

::: notes

``` { .haskell }
data Fix f = In (f (Fix f))
```

:::


## Id returns

``` { .haskell }
data IdType = UserId | CarId

data Id (a :: IdType) = Id

processUser :: Id 'UserId -> IO ()
processUser id = ...

processCar :: Id 'CarId -> IO ()
processCar id = ...
```


## Data kinds literals

In `GHC.TypeLits` you can find:

* `Nat`
* `Symbol`
* Conversion functions
* Type classes
* And many more


## Data kinds literals examples

``` { .haskell }
foo :: Maybe "bar"
foo2 :: Maybe 10
```

``` { .haskell }
Maybe ("bar" :: Symbol)
Maybe (10 :: Nat)
```


## Data kinds usage

* EDSLs e.g. Servant
* Type level numeric checks e.g. easytensor
* guarantee well-formedness


## Multi-parameter type classes

Requires extension `MultiParamTypeClasses`

``` { .haskell }
class Monad m => VarMonad m v where
    new :: a -> m (v a)
    get :: v a -> m a
    put :: v a -> a -> m ()

instance VarMonad IO IORef where ...
instance VarMonad (ST s) (STRef s) where ...
```


## Functional Dependences

* Requires extension `FunctionalDependencies`
* Allows as to specify dependencies between the parameters of a multiple
  parameter class


##  Coll 1

``` { .haskell }
class Coll s a where
    empty :: s
    insert :: a -> s -> s

instance Coll [Int] Double where
    empty = []
    insert a s = cons (fromIntegral a) s
```

``` { .haskell }
foo = empty
```


## Coll 2

``` { .haskell }
class Coll s a | s -> a where
    empty :: s
    insert :: a -> s -> s

instance Coll [Int] Double where
    empty = []
    insert a s = cons (fromIntegral a) s
```

``` { .haskell }
foo = empty
```

## Another example

``` { .haskell }
class D a b | a -> b where

-- Ok
instance D Bool Int where ...

-- Bad
instance D Bool Char where ...
```

``` { .haskell }
class E a b | a -> b, b -> a where

-- Ok
instance E Int Char where ...
instance E Bool Double where ...
instance E Double Bool where ...

-- Bad
instance E Int Word32 where ...
instance E Char Double where ...
```


## Type families


## Questions?

* Thank you for your attention.
* Slides will be available at [github.com/siprj/presentations](https://github.com/siprj/presentations)
