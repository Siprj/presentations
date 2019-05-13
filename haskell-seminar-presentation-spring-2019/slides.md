---
title: Real world Haskell
author: Jan Šipr <br/>[github.com/siprj](https://github.com/siprj)

date: 13 May 2018
---

## Something about me

* C++ - 4 years
* Haskell - 2 years
* No formal education in abstract mathematics ;)
* Work on some haskell packages (hsua, hlint-test, freer-effects)

## Why functional programming matters

* Functions are first class citizens
* Partial application
* Immutability
* Sum types
* Laziness
* Pattern matching
* Mostly strongly typed
* [Why functional programming matters](https://www.youtube.com/watch?v=XrNdvWqxBvA)

## Academic haskell vs. real world haskell

* There is not much deference

## Examples of some haskell projects

* Universal document converter [pandoc](https://pandoc.org/)
* Distributed file synchronization system [git-annex](https://git-annex.branchable.com/)
* Quake like game [Frag](https://github.com/rainbyte/frag)
* Whole haskell infrastructure
    * Hackage
    * Hoogle
    * Stackage
    * GHC

## Ixcom - Story of our first product

* Telephony exchange
* 70000 loc
* Bunch of APIs: REST API, Json RPC
* DSL for phone number description
* Build on top of asterisk
* The core never crashed

## Vision - Story of our second product

* Image processing
* Extremely fast development
* Zero tests
* We are still waiting for first bug to arrive

## Functional jobs

* Haskell jobs in Czech Republic (not many)
* Haskell jobs in the world (lot of opportunities)

## Hiring haskellers

* Easy to find
* People are over qualified
* People accept lower salary

## Great books

* [Haskell Programming from first principles](http://haskellbook.com/)
* [Real World Haskell](http://book.realworldhaskell.org/)
* [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)

## News and connections

* [r/haskell](https://www.reddit.com/r/haskell/)
* IRC channels on freenode
* [https://fpchat-invite.herokuapp.com/](https://fpchat-invite.herokuapp.com/)
* [pair-programming](https://github.com/Wizek/haskell-pair-programming)

## Invitation to FPBrno

* [meetup.com/fpbrno/](https://www.meetup.com/fpbrno/)

## Other areas where haskell can be used

* [clash](http://www.clash-lang.org/) - compilation haskell on FPGA
* [ivory-tower](https://ivorylang.org/ivory-introduction.html) - EDSL for C -> embedded devices
* quantum computers

## Some useful libraries

* acid
* aeson
* containers
* lenses
* megaparsec
* monad-logger
* mtl
* persistent
* servant
* template-haskell

## Servant example

~~~ { .haskell file=Fruit1.hs }
type ItemApi =
    -- GET /item
    "item" :> Get '[JSON] [Item] :<|>
    -- GET /item/:itemId
    "item" :> Capture "itemId" Integer :> Get '[JSON] Item

getItems :: Handler [Item]
getItems = return [exampleItem]

getItemById :: Integer -> Handler Item
getItemById = \case
    0 -> return exampleItem
    _ -> throwE err404

server :: Server ItemApi
server = getItems :<|> getItemById
~~~

Taken form [example-servant-minimal](https://github.com/haskell-servant/example-servant-minimal).

## Questions?

* Thank you for your attention.
* Slides will be available at [github.com/siprj/haskell-seminar-presentation-spring-2018](https://github.com/siprj/haskell-seminar-presentation-spring-2018)
