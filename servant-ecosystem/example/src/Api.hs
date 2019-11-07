{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}


module Api
    ( Api
    , User
    , Book
    )
  where

import Data.Aeson
import Data.Aeson.TH
import Data.String
import Data.Text
import Servant.API
import Text.Read
import Text.Show


data Book = Book
    { name :: Text
    , note :: Text
    , numberOfPages :: Text
    }
  deriving (Show, Read)

$(deriveJSON defaultOptions ''Book)

newtype User = User
    { userName :: String
    }
  deriving (Show, Read)

type Api =
    "books" :> BasicAuth "our-realm" User :> Post '[JSON] Book
    :<|> "books" :> Get '[JSON] [Book]
    :<|> "books" :> Capture "bookName" :> Get '[JSON] Book
