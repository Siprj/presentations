{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Api
    ( Api
    , OurApi
    , SwaggerApi
    , User(..)
    , Book(..)
    , api
    , ourApi
    )
  where

import Data.Aeson
import Data.Int
import Data.Proxy
import Data.String
import Data.Text
import GHC.Generics
import Servant.API
import Servant.Swagger.UI
import Text.Read
import Text.Show


data Book = Book
    { name :: Text
    , note :: Text
    , numberOfPages :: Int
    }
  deriving (Show, Read, Generic)

instance ToJSON Book
instance FromJSON Book

newtype User = User
    { userName :: String
    }
  deriving (Show, Read)

type OurApi =
    "books" :> BasicAuth "our-realm" User :> ReqBody '[JSON] Book :> PutNoContent '[JSON] NoContent
    :<|> "books" :> Get '[JSON] [Book]
    :<|> "books" :> Capture "bookName" Text :> Get '[JSON] Book

ourApi :: Proxy OurApi
ourApi = Proxy

type SwaggerApi = SwaggerSchemaUI "swagger-ui" "swagger.json"

api :: Proxy Api
api = Proxy

type Api = OurApi :<|> SwaggerApi
