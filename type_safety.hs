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
import Data.Type.Bool (If)
import Data.Type.Equality
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

-- this doesn't type check
-- testReverse' :: Check (Reverse '[1, 2, 3]) '[3, 2, 1,0]
-- testReverse' = Check

-- Insert x into a sorted type-level list of Nats
type family Insert (x :: Nat) (xs :: [Nat]) :: [Nat] where
  Insert x '[] = '[x]
  Insert x (y ': ys) =
    If
      (CmpNat x y == 'LT)
      (x ': y ': ys)
      (y ': Insert x ys)

type family Sort (xs :: [Nat]) where
  Sort '[] = '[]
  Sort (x ': xs) = Insert x (Sort xs)

testSort :: Check (Sort '[4, 2, 1, 3]) '[1, 2, 3, 4]
testSort = Check

type family Filter (x :: a) (xs :: [a]) :: [a] where
  Filter _ '[] = '[]
  Filter a (x ': xs) =
    If
      (x == a)
      (Filter a xs)
      (x ': Filter a xs)

testFilter :: Check (Filter 4 '[1, 4, 2, 4, 3, 4, 5]) '[1, 2, 3, 5]
testFilter = Check

type family Nub (xs :: [Nat]) :: [Nat] where
  Nub '[] = '[]
  Nub (x ': xs) = x ': Nub (Filter x xs)

testNub :: Check (Nub '[1, 2, 1, 3, 1, 4, 2, 2, 2, 3, 2, 4]) '[1, 2, 3, 4]
testNub = Check

main :: IO ()
main = do
  printLength (Proxy :: Proxy L)
  printLength (Proxy :: (Proxy (Float ': L)))
