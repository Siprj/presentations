---
title: How to be a type magician in haskell
author: Jan Šipr <br/>[github.com/siprj](https://github.com/siprj)

// TODO: date ....
date: 
---


## Agenda

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

~~~ { .haskell }
????
    Foo a :: * -> *
~~~


## Data promotion

* TODO:


## Data kinds usage

* TODO: Id
* TODO: value restriction -> https://wiki.haskell.org/Phantom_type


## Questions?

* Thank you for your attention.
* Slides will be available at [github.com/siprj/presentations](https://github.com/siprj/presentations)
