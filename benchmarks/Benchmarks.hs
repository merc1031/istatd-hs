{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main
import Criterion.Types

import Istatd.Istatd

main :: IO ()
main = defaultMainWith (defaultConfig { reportFile = Just "/tmp/bench.html" }) benches

benches :: [Benchmark]
benches = [
    prefixBench
  , suffixBench
  , prefixSuffixBench
  , bufferBench5
  , bufferBench50
  ]


prefixBench :: Benchmark
prefixBench =
  let environment = mkNullRecorder >>= \sink -> mkFilterPipeline sink [mkFilterPrefix "HiImAPrefix"]
  in env environment $ \e -> bgroup "prefixBenches" [
      bench "prefixBench" $ nfIO $ do
        iWriteChan e $ IstatdDatum Counter "CounterName" 0 1
    ]

suffixBench :: Benchmark
suffixBench =
  let environment = mkNullRecorder >>= \sink -> mkFilterPipeline sink [mkFilterSuffix "HiImASuffix"]
  in env environment $ \e -> bgroup "suffixBenches" [
      bench "suffixBench" $ nfIO $ do
        iWriteChan e $ IstatdDatum Counter "CounterName" 0 1

    ]

prefixSuffixBench :: Benchmark
prefixSuffixBench =
  let environment = mkNullRecorder >>= \sink -> mkFilterPipeline sink [mkFilterSuffix "HiImASuffix", mkFilterPrefix "HiImAPrefix"]
  in env environment $ \e -> bgroup "prefixSuffixBench" [
      bench "prefixSuffixBench" $ nfIO $ do
        iWriteChan e $ IstatdDatum Counter "CounterName" 0 1

    ]

bufferBench5 :: Benchmark
bufferBench5 =
  let environment = mkNullRecorder >>= \sink -> mkFilterPipeline sink [mkBuffer 5]
  in env environment $ \e -> bgroup "bufferBenches" [
      bench "bufferBench5" $ nfIO $ do
        iWriteChan e $ IstatdDatum Counter "CounterName" 0 1

    ]

bufferBench50 :: Benchmark
bufferBench50 =
  let environment = mkNullRecorder >>= \sink -> mkFilterPipeline sink [mkBuffer 50]
  in env environment $ \e -> bgroup "bufferBenches" [
      bench "bufferBench50" $ nfIO $ do
        iWriteChan e $ IstatdDatum Counter "CounterName" 0 1

    ]

