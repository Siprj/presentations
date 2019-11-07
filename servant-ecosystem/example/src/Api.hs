{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Api
    ( Api
    , User(..)
    , Book(..)
    , api
    )
  where

import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Data.Int
import Data.Proxy
import Data.String
import Data.Text
import Servant.API
import Servant.Docs
import Servant.Docs.Internal
import Text.Read
import Text.Show


data Book = Book
    { name :: Text
    , note :: Text
    , numberOfPages :: Int
    }
  deriving (Show, Read, Generic)

$(deriveJSON defaultOptions ''Book)

newtype User = User
    { userName :: String
    }
  deriving (Show, Read)

api :: Proxy Api
api = Proxy

type Api =
    Summary "Create book" :> "books"
        :> BasicAuth "our-realm" User
        :> ReqBody '[JSON] Book
        :> PutNoContent '[JSON] NoContent
    :<|> Summary "List all books"
        :> "books"
        :> Get '[JSON] [Book]
    :<|> Summary "Get book by name" :> "books"
        :> Capture "bookName" Text
        :> Get '[JSON] Book

instance ToSample Book where
    toSamples _ = singleSample (Book "name" "some note" 10)

instance ToCapture (Capture "bookName" Text) where
    toCapture _ = DocCapture "book name" "name of the book you want to retrieve"

instance ToAuthInfo (BasicAuth "our-realm" User) where
    toAuthInfo _ = DocAuthentication "basic authentication"
        "provide username and password"
