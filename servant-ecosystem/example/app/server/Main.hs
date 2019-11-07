{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Data.Bool
import Data.Eq
import qualified Data.Foldable as F
import Data.Function
import Data.IORef
import Data.Maybe
import Data.Text
import Network.Wai.Handler.Warp
import Servant
import System.IO

import Api


data HandlerCfg = HandlerCfg
    { appState :: IORef [Book]
    }

type AppHandler = ReaderT HandlerCfg Handler
type AppServer api = ServerT api AppHandler

createBook :: User -> Book -> AppHandler NoContent
createBook User{..} book =
    if userName /= "admin"
        then throwError err403
        else do
            HandlerCfg{..} <- ask
            void . liftIO $ modifyIORef' appState $ \books -> book : books
            pure NoContent

listBooks :: AppHandler [Book]
listBooks = do
    HandlerCfg{..} <- ask
    liftIO $ readIORef appState

getBookByName :: Text -> AppHandler Book
getBookByName bookName = do
    HandlerCfg{..} <- ask
    books <- liftIO $ readIORef appState
    maybe (throwError err404) pure $ F.find (\Book{..} -> name == bookName) books

server :: AppServer Api
server = createBook :<|> listBooks :<|> getBookByName

checkUser :: BasicAuthData -> IO (BasicAuthResult User)
checkUser (BasicAuthData username password) =
    if username == "admin" && password == "servant"
        then return (Authorized (User "admin"))
        else return Unauthorized

authCheck :: BasicAuthCheck User
authCheck = BasicAuthCheck checkUser

basicAuthContext :: Context (BasicAuthCheck User ': '[])
basicAuthContext = authCheck :. EmptyContext

nat :: HandlerCfg -> AppHandler a -> Handler a
nat cfg x = runReaderT x cfg

app :: HandlerCfg -> Application
app cfg = serveWithContext api basicAuthContext
    $ hoistServerWithContext api (Proxy :: Proxy '[BasicAuthCheck User])
        (nat cfg) server

main :: IO ()
main = do
    state <- newIORef []
    run 8080 (app $ HandlerCfg state)
