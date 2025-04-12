{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind (Type)
import Data.Proxy
import GHC.TypeLits

-- Type-level Length function
type family Length (xs :: [k]) :: Nat where
  Length '[] = 0
  Length (_ ': xs) = 1 + Length xs

-- Print the length at runtime
printLength :: forall (xs :: [Type]). KnownNat (Length xs) => Proxy xs -> IO ()
printLength _ = print $ natVal @(Length xs) Proxy

type Reverse xs = ReverseAcc xs '[]

type family ReverseAcc (xs :: [k]) (acc :: [k]) :: [k] where
  ReverseAcc '[] acc = acc
  ReverseAcc (x ': xs) acc = ReverseAcc xs (x ': acc)

data Check :: [k] -> [k] -> Type where
  Check :: Check a a

type L = '[Bool, String, Integer]

-- this only compyles if the type Check exists - which means that the first type argument should be the same as the second type argument
testReverse :: Check (Reverse L) '[Integer, String, Bool]
testReverse = Check

testReverse' :: Check (Reverse '[1, 2, 3]) '[3, 2, 1]
testReverse' = Check

main :: IO ()
main = do
  printLength (Proxy :: Proxy L)
  printLength (Proxy :: (Proxy (Float ': L)))
