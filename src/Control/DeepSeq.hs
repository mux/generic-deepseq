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
  ( DeepSeq(..)
  , deepseq
  , ($!!)
  , force
  ) where

import Data.Int
import Data.Word
import Data.Complex
import Data.Ratio
import GHC.Generics

class DeepSeq a where
  -- | Evaluate its argument to normal form, and then return '()'.
  rnf :: a -> ()
  default rnf :: (Generic a, GDeepSeq (Rep a)) => a -> ()
  rnf = grnf . from

-- Instances for primitive types.
instance DeepSeq Bool     where rnf x = x `seq` ()
instance DeepSeq Char     where rnf x = x `seq` ()
instance DeepSeq Double   where rnf x = x `seq` ()
instance DeepSeq Float    where rnf x = x `seq` ()
instance DeepSeq Int      where rnf x = x `seq` ()
instance DeepSeq Word     where rnf x = x `seq` ()
instance DeepSeq Integer  where rnf x = x `seq` ()
instance DeepSeq Ordering where rnf x = x `seq` ()
instance DeepSeq ()       where rnf x = x `seq` ()

instance DeepSeq Int8     where rnf x = x `seq` ()
instance DeepSeq Int16    where rnf x = x `seq` ()
instance DeepSeq Int32    where rnf x = x `seq` ()
instance DeepSeq Int64    where rnf x = x `seq` ()

instance DeepSeq Word8    where rnf x = x `seq` ()
instance DeepSeq Word16   where rnf x = x `seq` ()
instance DeepSeq Word32   where rnf x = x `seq` ()
instance DeepSeq Word64   where rnf x = x `seq` ()

instance DeepSeq (a -> b) where rnf x = x `seq` ()

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

-- We cannot derive an instance for Ratio automatically, because it is an
-- abstract datatype (it doesn't export its constructors), and doesn't provide
-- a Generic instance either.
instance (Integral a, DeepSeq a) => DeepSeq (Ratio a) where
  rnf x = rnf (denominator x) `seq` rnf (numerator x)

-- We should be able to derive an instance for Complex a automatically, but it
-- turns out GHC 7.4.1 panics if we do that, so we provide an explicit instance
-- for now.
instance DeepSeq a => DeepSeq (Complex a) where
  rnf (x :+ y) = rnf x `seq` rnf y

class GDeepSeq f where
  grnf :: f a -> ()

instance GDeepSeq V1 where
  grnf _  = undefined

instance GDeepSeq U1 where
  -- It is important to pattern match on the U1 constructor here. If we didn't
  -- and used an underscore pattern, the following equality wouldn't hold:
  --   x `deepseq` () = x `seq` () = _|_
  --     where x = undefined :: T
  -- with T being unit-shaped type with a DeepSeq instance.
  grnf U1 = ()

instance DeepSeq a => GDeepSeq (K1 i a) where
  grnf = rnf . unK1

instance GDeepSeq a => GDeepSeq (M1 i c a) where
  grnf = grnf . unM1

instance (GDeepSeq a, GDeepSeq b) => GDeepSeq (a :*: b) where
  grnf (x :*: y) = grnf x `seq` grnf y

instance (GDeepSeq a, GDeepSeq b) => GDeepSeq (a :+: b) where
  grnf (L1 x) = grnf x
  grnf (R1 x) = grnf x

infixr 0 $!!

-- | The deep analogue of '$!'.
($!!) :: DeepSeq a => (a -> b) -> a -> b
f $!! x = x `deepseq` f x

-- | Evaluates its first argument to normal form, and then returns its
-- second argument as the result.
deepseq :: DeepSeq a => a -> b -> b
deepseq x y = rnf x `seq` y

-- | Evaluates its argument to normal form, and then return it.
force :: DeepSeq a => a -> a
force x = x `deepseq` x
