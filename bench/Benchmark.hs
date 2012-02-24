{-# LANGUAGE PackageImports #-}
module Main where

import Criterion.Main
import qualified "deepseq" Control.DeepSeq as D1
import qualified "generic-deepseq" Control.DeepSeq as D2

list :: Int -> [Int]
list n = [1..10^n]

main :: IO ()
main = defaultMain
         [ bgroup "deepseq"
             [ bench "10^5 elems" $ whnf (uncurry D1.deepseq) (list 5,())
             , bench "10^6 elems" $ whnf (uncurry D1.deepseq) (list 6,())
             , bench "10^7 elems" $ whnf (uncurry D1.deepseq) (list 7,())
             ]
         , bgroup "generic-deepseq"
             [ bench "10^5 elems" $ whnf (uncurry D2.deepseq) (list 5,())
             , bench "10^6 elems" $ whnf (uncurry D2.deepseq) (list 6,())
             , bench "10^7 elems" $ whnf (uncurry D2.deepseq) (list 7,())
             ]
         ]
