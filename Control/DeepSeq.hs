{-# LANGUAGE DefaultSignatures, FlexibleContexts, TypeOperators #-}

-- |
-- Module:  Control.DeepSeq
-- Copyright:   (c) 2012, Maxime Henrion
-- License:     BSD-style (see the LICENSE file)
--
-- Maintainer:  Maxime Henrion <mhenrion@gmail.com>
-- Stability:   stable
-- Portability: portable
--
-- This module provides a 'deepseq' function for fully evaluating data
-- structures (that is, evaluating to \"Normal Form\", and not just up to
-- \"Head Normal Form\" like 'seq' does).
--
-- It uses the "GHC.Generics" framework so that you can generate instances
-- for your datatypes without having to provide an implementation.
--
module Control.DeepSeq
  (
    DeepSeq(..)
    -- * Convenience functions
  , ($!!)
  , rnf
  , force
  ) where

import Data.Int
import Data.Word
import GHC.Generics

class DeepSeq a where
  -- | Evaluates its first argument to normal form, and then returns its
  -- second argument as the result.
  deepseq :: a -> b -> b
  default deepseq :: (Generic a, GDeepSeq (Rep a)) => a -> b -> b
  deepseq = gdeepseq . from

instance DeepSeq Bool     where deepseq = seq
instance DeepSeq Char     where deepseq = seq
instance DeepSeq Double   where deepseq = seq
instance DeepSeq Float    where deepseq = seq
instance DeepSeq Int      where deepseq = seq
instance DeepSeq Word     where deepseq = seq
instance DeepSeq Integer  where deepseq = seq
instance DeepSeq Ordering where deepseq = seq
instance DeepSeq ()       where deepseq = seq

instance DeepSeq Int8     where deepseq = seq
instance DeepSeq Int16    where deepseq = seq
instance DeepSeq Int32    where deepseq = seq
instance DeepSeq Int64    where deepseq = seq

instance DeepSeq Word8    where deepseq = seq
instance DeepSeq Word16   where deepseq = seq
instance DeepSeq Word32   where deepseq = seq
instance DeepSeq Word64   where deepseq = seq

instance (DeepSeq a, DeepSeq b) => DeepSeq (a,b)
instance (DeepSeq a, DeepSeq b, DeepSeq c) => DeepSeq (a,b,c)
instance (DeepSeq a, DeepSeq b, DeepSeq c, DeepSeq d) => DeepSeq (a,b,c,d)
instance (DeepSeq a, DeepSeq b, DeepSeq c, DeepSeq d, DeepSeq e) =>
          DeepSeq (a,b,c,d,e)
instance (DeepSeq a, DeepSeq b, DeepSeq c, DeepSeq d, DeepSeq e, DeepSeq f) =>
          DeepSeq (a,b,c,d,e,f)
instance (DeepSeq a, DeepSeq b, DeepSeq c, DeepSeq d, DeepSeq e, DeepSeq f,
          DeepSeq g) => DeepSeq (a,b,c,d,e,f,g)

instance DeepSeq a => DeepSeq [a]
instance DeepSeq a => DeepSeq (Maybe a)
instance (DeepSeq a, DeepSeq b) => DeepSeq (Either a b)

class GDeepSeq f where
  gdeepseq :: f a -> b -> b

instance GDeepSeq V1 where
  gdeepseq = flip const

instance GDeepSeq U1 where
  gdeepseq = flip const

instance DeepSeq a => GDeepSeq (K1 i a) where
  gdeepseq = deepseq . unK1

instance GDeepSeq a => GDeepSeq (M1 i c a) where
  gdeepseq = gdeepseq . unM1

instance (GDeepSeq a, GDeepSeq b) => GDeepSeq (a :*: b) where
  gdeepseq (x :*: y) = gdeepseq x . gdeepseq y

instance (GDeepSeq a, GDeepSeq b) => GDeepSeq (a :+: b) where
  gdeepseq (L1 x) = gdeepseq x
  gdeepseq (R1 x) = gdeepseq x

infixr 0 $!!

($!!) :: DeepSeq a => (a -> b) -> a -> b
f $!! x = x `deepseq` f x

rnf :: DeepSeq a => a -> ()
rnf x = x `deepseq` ()

force :: DeepSeq a => a -> a
force x = x `deepseq` x
