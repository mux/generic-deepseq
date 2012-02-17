{-# LANGUAGE TypeOperators #-}
module DeepSeq.Internal where

import GHC.Generics

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
