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


data Format
    = Lit Symbol
    | D
    | S

type Msg = [Lit "Number: ", D, Lit " and string: ", S]

class FormatFunction (format :: [Format]) fun | format -> fun where
    formatFunction :: String -> fun

instance FormatFunction '[] String where
    formatFunction = id

instance FormatFunction format fun
    => FormatFunction (S ': format) (String -> fun)
  where
    formatFunction str s = formatFunction @format $ str <> s

instance FormatFunction format fun
    => FormatFunction (D ': format) (Int -> fun)
  where
    formatFunction str i = formatFunction @format $ str <> show i

instance (FormatFunction format fun, KnownSymbol symbol)
    => FormatFunction (Lit symbol ': format) fun
  where
    formatFunction str =
        formatFunction @format $ str <> symbolVal (Proxy @symbol)

printf :: forall format fun. FormatFunction format fun => fun
printf = formatFunction @format ""


type family FromString (a :: [Symbol]) :: [Format] where
    FromString ("%" ': "d" ': xs) = D ': FromString xs
    FromString ("%" ': "s" ': xs) = S ': FromString xs
    FromString (x ': xs) = FromLiteral x (FromString xs)
    FromString '[] = '[]

type family FromLiteral (a :: Symbol) (b :: [Format]) :: [Format] where
    FromLiteral c (Lit s ': xs) = Lit (AppendSymbol c s) ': xs
    FromLiteral c xs = Lit c ': xs

printf2
    :: forall format list parsed fun
    . ( Listify format list
    , FromString list ~ parsed
    , FormatFunction parsed fun
    )
    => fun
printf2 = formatFunction @parsed ""

main :: IO ()
main = do
    putStrLn $ printf @Msg 10 "Hello World!"
    putStrLn $ printf2 @"Number: %d and string: %s" 10 "Hello World!"
