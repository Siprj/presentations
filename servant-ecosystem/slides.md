---
title: Servant Ecosystem
subtitle: 
author: Jan Šipr <br/>[github.com/siprj](https://github.com/siprj)

date: Nov 3, 2019
---


## Agenda

* Web API as type
* Serving an API
* Querying an API
* Generating non haskell client
* Authentication
* List of some interested packages
* Example of CLI application


## What is servant?

Servant is an EDSL for declaring web APIs at the type-level, and then
using this declaration to:

* write servers,
* obtain client functions,
* generate non haskell clients,
* generate documentation,
* and many more.


## Extensions??!

* DataKinds
* FlexibleContexts
* FlexibleInstances
* MultiParamTypeClasses
* RankNTypes
* ScopedTypeVariables
* TypeFamilies
* TypeOperators
* UndecidableInstances


## Simple example

``` { .haskell }
data User = User {
  name :: String,
  age :: Int
}

type UserAPI = "users" :> Get '[JSON] [User]

type UserAPI5 =
   "users" :> Get '[JSON] User
   :<|> "user" :> Capture "userId" Integer :> Get '[JSON] User
   :<|> "user" :> Capture "userId" Integer
       :> DeleteNoContent '[JSON] NoContent
```


## Composition combinators

Two combinator used for composing

* `:>` this is equivalent to '/' in URL
* `:<|>` combines alternative endpoints

::: notes

* You can notice similarity to Alternative operator.

:::


## Verb type combinators

`Verb` is a general type for representing HTTP verbs (a.k.a. methods).

``` { .haskell }
data Verb (method :: k1) (statusCode :: Nat)
    (contentTypes :: [*]) (a :: *)
```

``` { .haskell }
type Post = Verb POST 200
-- Post :: (contentTypes :: [*]) (a :: *)
```

``` { .haskell }
Post '[JSON] User
```


## Predefined methods (verbs)

* `Get`
* `Put`
* `Post`
* `Patch`
* `PostCreated`
* and many more


## Capture

``` { .haskell }
data Capture (sym :: Symbol) (a :: *)
```

``` { .haskell }
-- equivalent to 'GET /users/userid'
type Api = "user" :> Capture "userId" Integer
    :> Get '[JSON] User
```


## Query

``` { .haskell }
data QueryParam (sym :: Symbol) a
data QueryParams (sym :: Symbol) a
data QueryFlag (sym :: Symbol)
```

``` { .haskell }
data SortBy = Age | Name

-- equivalent to 'GET /users?sortby=(age,name)/'
type Api = "users" :> QueryParam "sortby" SortBy 
    :> Get '[JSON] User
```


## Request body

``` { .haskell }
type Api = "user" :> "create" 
    :> ReqBody '[JSON] User :> Post '[JSON] UserId
```

``` { .haskell }
data ReqBody (contentTypes :: [*]) (a :: *)
```


## Authentication

``` { .haskell }
type Api = "user" :> BasicAuth "foo-realm" UserId
    :> Get '[JSON] PrivateData
```

``` { .haskell }
type Api = "user" :> AuthProtect "cookie-auth"
    :> Get '[JSON] PrivateData
```


## Serving API

``` { .haskell }
serve :: HasServer api '[]
    => Proxy api -> Server api -> Application
```


## Serving API

``` { .haskell }
type Api = "books" :> Get '[JSON] [Book]
    :<|> Capture "bookId" :> Get '[JSON] Book

apiProxy :: Proxy Api
apiProxy = Proxy

findBook :: ExceptT ServerError IO Book
findBook bookId = pure $ Book

handler :: Server Api
handler = pure globalListOfBooks
    :<|> findBook

serverApp :: Application
serverApp = serve apiProxy server

main = run 8080 server
```


## Content types

* `JSON`
* `PlainText`
* `FormUrlEncoded`
* `OctetStream`
* and many more in separate packages


## Live coding session

...

## Questions?

Thank you for your attention.
