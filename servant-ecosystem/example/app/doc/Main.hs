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
import Servant.Docs
import Servant
import System.IO

import Api


main :: IO ()
main = putStrLn . markdown $ docs api
