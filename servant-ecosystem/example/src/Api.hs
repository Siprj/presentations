{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Api
    ( Api
    , User(..)
    , Book(..)
    , api
    )
  where

import Data.Aeson
import Data.Aeson.TH
import Data.Int
import Data.Proxy
import Data.String
import Data.Text
import Servant.API
import Text.Read
import Text.Show


data Book = Book
    { name :: Text
    , note :: Text
    , numberOfPages :: Int
    }
  deriving (Show, Read)

$(deriveJSON defaultOptions ''Book)

newtype User = User
    { userName :: String
    }
  deriving (Show, Read)

api :: Proxy Api
api = Proxy

type Api =
    "books" :> BasicAuth "our-realm" User :> ReqBody '[JSON] Book :> PutNoContent '[JSON] NoContent
    :<|> "books" :> Get '[JSON] [Book]
    :<|> "books" :> Capture "bookName" Text :> Get '[JSON] Book
