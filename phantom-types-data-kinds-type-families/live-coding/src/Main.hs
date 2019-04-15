{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Function (($))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Symbol.Utils (Listify)
import GHC.TypeLits (AppendSymbol, KnownSymbol, Symbol, symbolVal)
import System.IO (IO, putStrLn)
import Text.Show (show)


main :: IO ()
main = putStrLn "Hello world!"
