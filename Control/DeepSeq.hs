{-# LANGUAGE DefaultSignatures, FlexibleContexts, TypeOperators #-}
module Control.DeepSeq
  ( DeepSeq(..)
  ) where

import GHC.Generics

class DeepSeq a where
  deepseq :: a -> b -> b
  default deepseq :: (Generic a, GDeepSeq (Rep a)) => a -> b -> b
  deepseq = gdeepseq . from

instance DeepSeq Bool where deepseq = seq
instance DeepSeq Char where deepseq = seq
instance DeepSeq Double where deepseq = seq
instance DeepSeq Float where deepseq = seq
instance DeepSeq Int where deepseq = seq
instance DeepSeq Ordering where deepseq = seq
instance DeepSeq () where deepseq = seq

instance (DeepSeq a, DeepSeq b) => DeepSeq (a,b)
instance (DeepSeq a, DeepSeq b, DeepSeq c) => DeepSeq (a,b,c)
instance (DeepSeq a, DeepSeq b, DeepSeq c, DeepSeq d) => DeepSeq (a,b,c,d)
instance (DeepSeq a, DeepSeq b, DeepSeq c, DeepSeq d, DeepSeq e) =>
          DeepSeq (a,b,c,d,e)
instance (DeepSeq a, DeepSeq b, DeepSeq c, DeepSeq d, DeepSeq e, DeepSeq f) =>
          DeepSeq (a,b,c,d,e,f)
instance DeepSeq a => DeepSeq [a]

class GDeepSeq f where
  gdeepseq :: f a -> b -> b

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
