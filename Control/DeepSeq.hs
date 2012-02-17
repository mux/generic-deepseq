{-# LANGUAGE DefaultSignatures, FlexibleContexts #-}
module Control.DeepSeq
  ( DeepSeq(..)
  ) where

import GHC.Generics
import Control.DeepSeq.Internal

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
