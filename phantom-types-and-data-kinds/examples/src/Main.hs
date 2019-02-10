{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Function (($))
import Data.Monoid ((<>))
import Text.Show (show)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Data.Proxy (Proxy(Proxy))
import System.IO (IO, putStrLn)


data Format
    = Lit Symbol
    | D
    | S

class FormatFunction (format :: [Format]) fun | format -> fun where
    formatFunction :: String -> fun

instance FormatFunction '[] String where
    formatFunction = id

instance (FormatFunction format fun) => FormatFunction (S ': format) (String -> fun) where
    formatFunction str i =
        formatFunction @format $ str <> show i

instance FormatFunction format fun => FormatFunction (D ': format) (Int -> fun) where
    formatFunction str i =
        formatFunction @format $ str <> show i

instance (FormatFunction format fun, KnownSymbol symbol) => FormatFunction (Lit symbol ': format) fun where
    formatFunction str =
        formatFunction @format $ str <> symbolVal (Proxy @symbol)

printf :: forall format fun. FormatFunction format fun => fun
printf = formatFunction @format ""


type Msg = [Lit "Number: ", D, Lit "\nand string: ", S]

main :: IO ()
main = putStrLn $ printf @Msg 10 "Hello World!"
