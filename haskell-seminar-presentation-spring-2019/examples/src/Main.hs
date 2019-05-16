{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Aeson
import Data.Monoid
import Data.SafeCopy
import Data.Typeable
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import System.Environment


data Book = Book
    { name :: String
    , autor :: String
    }
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Book)

data BookStore = BookStore [Book]
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''BookStore)

addBook :: Book -> Update BookStore ()
addBook newValue =
    modify (\(BookStore state) -> BookStore $ newValue : state)

listAllBooks :: Query BookStore [Book]
listAllBooks = do
    BookStore books <- ask
    pure books

$(makeAcidic ''BookStore ['addBook, 'listAllBooks])

main :: IO ()
main = do
    acidState <- openLocalState (BookStore [])
    args <- getArgs
--    if null args
--        then do
--            string <- show <$> query acidState ListAllBooks
--            putStrLn $ "The state is: " ++ string
--        else do
--          update acidState . AddBook $ Book (args !! 0) (args !! 1)
--          putStrLn "The state has been modified!"
    run 8080 $ app acidState

type BookStoreAPI
    = "listBooks" :> Get '[JSON] [Book]
    :<|> "addBook" :> Capture "name" String :> Capture "autor" String :> Post '[JSON] ()
    :<|> "hello" :> Get '[JSON] String

instance FromJSON Book where
    parseJSON = withObject "Book" $ \o -> Book
        <$> o .: "x"
        <*> o .: "y"

instance ToJSON Book where
    toJSON Book{..} = object
        [ "name" .= name
        , "autor" .= autor
        ]

server :: AcidState BookStore -> Server BookStoreAPI
server acidState = listBooks :<|> addBook :<|> hello
  where
    listBooks = do
        liftIO $ query acidState ListAllBooks
    addBook name autor = do
        liftIO . update acidState . AddBook $ Book name autor
    hello = do
        pure "Hello world"

bookStoreApi :: Proxy BookStoreAPI
bookStoreApi = Proxy

app :: AcidState BookStore -> Application
app acidState = serve bookStoreApi $ server acidState
