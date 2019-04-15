---
title: Encode your Semantics in Types
subtitle: Introduction to Type-level Programming in Haskell
author: Jan Šipr <br/>[github.com/siprj](https://github.com/siprj)

date: April 14, 2019
---


## Agenda

[//]: # (Do I need phantom types???)
* Phantom types
* Data kinds
* Multi-parameter type classes
* Functional dependences
* Type families
* Live coding session


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


## What can I do with phantom types?

``` { .haskell }
data Id a = Id Word64

type UserId = Id UserData

type = Map UserId UserData
```

## What can I do with phantom types?

``` { .haskell }
data Distance a = Distance Word64

addDistances :: Distance a -> Distance a
```

## What can I do with phantom types?

``` { .haskell }
data Proxy a = Proxy

serve :: HasServer api '[] => Proxy api -> Server api -> Application
```


## Before data kinds

``` { .haskell }
data Foo a = Bar | Baz
```

* `Foo a` is **type** constructor
* `Bar`, `Baz` are **data** constructors

::: notes

Before `data kinds`.

Explain what is `data constructor` and what is `type constructor`.

:::


## Data kinds

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


## Type literals

In `GHC.TypeLits` you can find:

* `Nat`
* `Symbol`
* Conversion functions
* Type classes
* And many more


## Type literals examples

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


##  Collection

``` { .haskell }
class Collection s a where
    empty :: s
    insert :: a -> s -> s

instance Collection [Int] Double where
    empty = []
    insert a s = cons (fromIntegral a) s
```

``` { .haskell }
foo = empty
```


## Collection

``` { .haskell }
class Collection s a | s -> a where
    empty :: s
    insert :: a -> s -> s

instance Collection [Int] Double where
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

* Requires extension `TypeFamilies`
* Data type family
* Type synonym family


## Where can we use type families

* They can appear inside type classes
* They can be defined on the toplevel
  * They can be closed
  * They can be opened


## Data type families on top level

``` { .haskell }
data family XList a

data instance XList Char = XCons !Char !(XList Char) | XNil
data instance XList () = XListUnit !Int
```


## Associated data type families

``` { .haskell }
class GMapKey k where
    data GMap k :: * -> *
    empty :: GMap k v
    lookup :: k -> GMap k v -> Maybe v

instance GMapKey Int where
    data GMap Int v = GMapInt (Data.IntMap.IntMap v)
    empty = GMapInt Data.IntMap.empty
    lookup k (GMapInt m) = Data.IntMap.lookup k m
```


## Where would I use data families

* In generic collections.
* When I need to associated type class and type.
* In protocol implementations (e.g. JSON RPC).
* And in many other cases.


## Open type synonym

``` { .haskell }
type family F a :: *

type instance F [Int] = Int
type instance F String = Char
```


## Closed type synonym

``` { .haskell }
type family F2 a where
  F2 (Maybe Int)  = Int
  F2 (Maybe Bool) = Bool
```


## Closed type synonym families and data kinds

``` { .haskell }
data Foo = I | D

type family Bar (a :: Foo) where
  Bar I = Int
  Bar D = Double
```


## Associated type synonym families

``` { .haskell}
class Collection ce where
    type Elem ce
    empty :: ce
    insert :: Elem ce -> ce -> ce

instance Collection [e] where
    type Elem [e]   = e
    empty           = []
    insert e l      = (e:l)
```


## Where would I use type synonym families

* EDSLs like (e.g. servant)
* Whenever you need function on type level.


## Live coding session

...

## Questions?

Thank you for your attention.
