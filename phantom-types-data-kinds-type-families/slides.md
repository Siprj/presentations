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
* Type Families
* Short live coding session


## What is a Phantom type?

A phantom type is a parametrised type whose parameters do not all appear on
the right-hand side of its definition.

~~~ { .haskell }
data Foo a = Foo Int
~~~


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


## This is not the Phantom type!!!

~~~ { .haskell }
type Foo a = String
~~~

::: notes

* Type alias is expanded before type check.

:::


## Kinds

* In type theory kind is type of a type.
* Exists only at compile time.

TODO: Describe difference between data constructor and type constructor.

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


## Data kinds literals

* In `GHC.TypeLits`
* `Nat`
* `Symbol`

~~~
foo :: Maybe "bar"
foo2 :: Maybe 10
~~~

::: notes

* Reference to easytensor

:::


## Custom data kind definition

Just like standard data structures:

~~~
data Foo = Bar | Baz Foo
~~~


## Promotion restrictions

* We only promote datatypes whose kinds are of the form `* -> ... -> * -> *`
* We do not promote datatypes whose constructors are kind polymorphic,
  involve constraints, or use existential quantification.

::: notes

data Fix f = In (f (Fix f))

:::


## Data promotion

* Data promotion is done with an apostrophe
* Data promotion is done with an apostrophe

~~~ { .haskell }
Int :: *
Maybe :: * -> *
Maybe Bool :: *
a -> a :: *
[] :: * -> *
(->) :: * -> * -> *
~~~


## Data kinds usage

* TODO: Id
* TODO: value restriction -> https://wiki.haskell.org/Phantom_type
* TODO: EDSL


## Multiparametric type classes


## Functional Dependences


## Type families


## Questions?

* Thank you for your attention.
* Slides will be available at [github.com/siprj/presentations](https://github.com/siprj/presentations)
