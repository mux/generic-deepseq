{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts #-}
module DeepSeq where

import GHC.Generics

class DeepSeq a where
  deepseq :: a -> b -> b
  default deepseq :: (Generic a, GDeepSeq (Rep a)) => a -> b -> b
  deepseq x = gdeepseq (from x)

instance DeepSeq Bool
instance DeepSeq Char
instance DeepSeq Double
instance DeepSeq Float
instance DeepSeq Int
instance DeepSeq Ordering
instance DeepSeq ()
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

instance GDeepSeq (K1 i a) where
  gdeepseq = seq

instance GDeepSeq a => GDeepSeq (M1 i c a) where
  gdeepseq = gdeepseq . unM1

instance (GDeepSeq a, GDeepSeq b) => GDeepSeq (a :*: b) where
  gdeepseq (x :*: y) = gdeepseq y . gdeepseq x

instance (GDeepSeq a, GDeepSeq b) => GDeepSeq (a :+: b) where
  gdeepseq (L1 x) = gdeepseq x
  gdeepseq (R1 x) = gdeepseq x
