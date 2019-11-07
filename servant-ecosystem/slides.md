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

handler :: Server Api
handler = pure bookList

apiProxy :: Proxy Api
apiProxy = Proxy

serverApp :: Application
serverApp = serve apiProxy server

main = run 8080 server
```


## Handlers

``` { .haskell }
type Api = "books" :> Get '[JSON] [Book]
    :<|> Capture "bookId" Int :> Get '[JSON] Book

handler :: Server Api
handler = (pure globalListOfBooks)
    :<|> findBook

apiProxy :: Proxy Api
apiProxy = Proxy

findBook :: Int -> ExceptT ServerError IO Book
findBook bookId = pure $ Book
```


## Handler types

``` { .haskell }
data QueryParam (sym :: Symbol) a
data Capture (sym :: Symbol) a

data Header' (mods :: [*]) (sym :: Symbol) a
type Header = Header' '[Optional, Strict]
```


## Handler types

``` { .haskell }
type Api = QueryParam "somethig" Text :> Capture "bookId" Int 
    :> Get '[JSON] Book

handler :: Maybe Text -> Int -> ServerT ServerError IO
```


## Content types

* `JSON`
* `PlainText`
* `FormUrlEncoded`
* `OctetStream`
* and many more in separate packages


## Content types

``` { .haskell }
type Api = Get '[JSON, FormUrlEncoded] Book
-- Book needs to have instances for:
--   * ToForm
--   * ToJson

-- In some cases Content Type forces return value.
type Api = Get '[PlainText] Text
type Api = Get '[PlainText] String
```


## Querying API

``` { .haskell }
client :: HasClient ClientM api
    => Proxy api -> Client ClientM api
```


## Query functions

``` { .haskell }
type API = 
    "book" :> Capture "bookId" Int :> Get '[JSON] Book
    :<|> "hello" :> QueryParam "name" String 
        :> Get '[JSON] HelloMessage

api :: Proxy Api
api = Proxy

book :<|> hello = client api

book :: Int -> ClientM Book
hello :: Maybe String -> ClientM HelloMessage
```

## Running query functions

``` { .haskell }
runClientM
    :: ClientM a
    -> ClientEnv
    -> IO (Either ClientError a)
```


## Running query functions

``` { .haskell }
book :: Int -> ClientM Book
hello :: Maybe String -> ClientM HelloMessage

run :: IO ()
run = do
    manager' <- newManager defaultManagerSettings
    _ <- runClientM (book 10)
        (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
```


## Non haskell clients

* servant-js
* servant-elm
* servant-purescript
* servant-ruby
* servant-kotlin
* ...


## Elm client

``` { .haskell }
data Book = Book
    { name :: String
    }

deriveBoth defaultOptions ''Book

type BooksApi = "books" :> Capture "bookId" Int :> Get '[JSON] Book

main :: IO ()
main =
  generateElmModuleWith
    defElmOptions ["Generated", "MyApi"]
    defElmImports "my-elm-dir" [DefineElm (Proxy :: Proxy Book)]
    (Proxy :: Proxy BooksApi)
```


## Elm client


``` { .haskell }
module Generated.MyApi exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

type alias Book  =
   { name: String
   }

jsonDecBook : Json.Decode.Decoder ( Book )
jsonDecBook =
   Json.Decode.succeed (\pname -> {name = pname})
   |> required "name" (Json.Decode.string)

jsonEncBook : Book -> Value
jsonEncBook  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   ]


getBooksByBookId : Int -> Http.Request Book
getBooksByBookId capture_bookId =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.absolute
                    [ "books"
                    , capture_bookId |> String.fromInt
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson <| jsonDecBook
            , timeout =
                Nothing
            , withCredentials =
                False
            }
```


## Basic Authentication

``` { .haskell }
type API = "public"  :> PublicAPI
    :<|> "private" :> BasicAuth "foo-realm" User :> Get '[JSON] Stuff

privateHandler :: User -> ServerT ServerError IO Stuff
privateHandler user = ...
```


## Basic Authentication

``` { .haskell }
data BasicAuthResult usr
  = Unauthorized
  | BadPassword
  | NoSuchUser
  | Authorized usr
  deriving (Eq, Show, Read, Generic, Typeable, Functor)

checkUser :: BasicAuthData -> IO (BasicAuthResult usr)
checkUser (BasicAuthData username password) = ...
```

## Basic Authentication

``` { .haskell }
serveWithContext
    :: HasServer api context
    => Proxy api
    -> Context context
    -> Server api
    -> Application
```


## Basic Authentication

``` { .haskell }
authCheck :: BasicAuthCheck User
authCheck = BasicAuthCheck checkUser

basicAuthContext :: Context (BasicAuthCheck User ': '[])
basicAuthContext = authCheck :. EmptyContext

main :: IO ()
main = run 8080 (serveWithContext api
    basicAuthServerContext basicAuthServer)
```

## Generalized authentication

``` { .haskell }
type Api = "user" :> AuthProtect "token-auth"
    :> Get '[JSON] PrivateData

-- authHandler :: Request -> ExceptT ServerError IO a
authHandler :: Request -> ExceptT ServerError IO AuthenticatedUser
authHandler req = ...

-- Type family representing authenticated user
-- for given authentication method
type instance AuthServerData (AuthProtect "cookie-auth") =
    AuthenticatedUser
```

## Generalized authentication

``` { .haskell }
authContext :: Context (BasicAuthCheck User ': '[])
authContext = mkAuthHandler authHandler :. EmptyContext
```


## Example

...


## Questions?

Thank you for your attention.
