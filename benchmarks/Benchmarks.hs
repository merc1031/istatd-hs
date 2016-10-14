{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import            Criterion.Main
import            Criterion.Types

import            Control.Monad.IO.Class
import            Control.DeepSeq           ( NFData )
import            Istatd.Istatd
import            Data.Proxy
import qualified  Istatd.Chan.ChanT         as ChanT
import qualified  Istatd.Chan.Chan          as Chan

type ChannelAtomic = Chan.InChanI IstatdDatum
type ChannelTVar = ChanT.InChanI IstatdDatum

main :: IO ()
main = defaultMainWith (defaultConfig { reportFile = Just "/tmp/bench.html" }) benches

benches :: [Benchmark]
benches = [
    bgroup "Chan" [
      prefixBench (Proxy :: Proxy ChannelAtomic)
    , suffixBench (Proxy :: Proxy ChannelAtomic)
    , prefixSuffixBench (Proxy :: Proxy ChannelAtomic)
    , bufferBench5 (Proxy :: Proxy ChannelAtomic)
    ]
  , bgroup "ChanT" [
      prefixBench (Proxy :: Proxy ChannelTVar)
    , suffixBench (Proxy :: Proxy ChannelTVar)
    , prefixSuffixBench (Proxy :: Proxy ChannelTVar)
    , bufferBench5 (Proxy :: Proxy ChannelTVar)
    ]
  ]


prefixBench :: ( NFData (cit IstatdDatum)
               , ChanLike cit cot IstatdDatum
               )
            => Proxy (cit IstatdDatum)
            -> Benchmark
prefixBench p =
  bgroup "prefixBenches" [
      let environment = mkNullRecorder >>= \(sink) -> mkFilterPipeline sink [mkFilterPrefix "HiImAPrefix"]
      in env environment $ \e -> bench "prefixBench" $ nfIO $ do
          writeChan p e
    , let environment = mkNullRecorder >>= \(sink) -> mkFilterPipeline sink [mkFilterPrefix "HiImAPrefixThatIsVeryVeryVeryVeryLongLongLong"]
      in env environment $ \e -> bench "prefixBenchLong" $ nfIO $ do
          writeChan p e
    , let environment = mkNullRecorder >>= \(sink) -> mkFilterPipeline sink [mkFilterPrefix "Hi", mkFilterPrefix "Im", mkFilterPrefix "A", mkFilterPrefix "Prefix"]
      in env environment $ \e -> bench "prefixBench4" $ nfIO $ do
          writeChan p e
    ]

suffixBench :: ( NFData (cit IstatdDatum)
               , ChanLike cit cot IstatdDatum
               )
            => Proxy (cit IstatdDatum)
            -> Benchmark
suffixBench p =
  bgroup "suffixBenches" [
      let environment = mkNullRecorder >>= \(sink) -> mkFilterPipeline sink [mkFilterSuffix "HiImASuffix"]
      in env environment $ \e -> bench "suffixBench" $ nfIO $ do
          writeChan p e

    , let environment = mkNullRecorder >>= \(sink) -> mkFilterPipeline sink [mkFilterSuffix "HiImASuffixThatIsVeryVeryVeryVeryLongLongLong"]
      in env environment $ \e -> bench "suffixBenchLong" $ nfIO $ do
          writeChan p e
    , let environment = mkNullRecorder >>= \(sink) -> mkFilterPipeline sink [mkFilterSuffix "Hi", mkFilterSuffix "Im", mkFilterSuffix "A", mkFilterSuffix "Suffix"]
      in env environment $ \e -> bench "suffixBench4" $ nfIO $ do
          writeChan p e
    ]

prefixSuffixBench :: ( NFData (cit IstatdDatum)
                     , ChanLike cit cot IstatdDatum
                     )
                  => Proxy (cit IstatdDatum)
                  -> Benchmark
prefixSuffixBench p =
  bgroup "prefixSuffixBenches" [
      let environment = mkNullRecorder >>= \(sink) -> mkFilterPipeline sink [mkFilterSuffix "HiImASuffix", mkFilterPrefix "HiImAPrefix"]
      in env environment $ \e -> bench "prefixSuffixBench" $ nfIO $ do
          writeChan p e

    ]

bufferBench5 :: ( NFData (cit IstatdDatum)
                , ChanLike cit cot IstatdDatum
                )
             => Proxy (cit IstatdDatum)
             -> Benchmark
bufferBench5 p =
  bgroup "bufferBenches" [
      let environment = mkNullRecorder >>= \(sink) -> mkFilterPipeline sink [mkBuffer 0]
      in env environment $ \e -> bench "bufferBench0" $ nfIO $ do
          writeChan p e
    , let environment = mkNullRecorder >>= \(sink) -> mkFilterPipeline sink [mkBuffer 1]
      in env environment $ \e -> bench "bufferBench1" $ nfIO $ do
          writeChan p e
    , let environment = mkNullRecorder >>= \(sink) -> mkFilterPipeline sink [mkBuffer 5]
      in env environment $ \e -> bench "bufferBench5" $ nfIO $ do
          writeChan p e
    , let environment = mkNullRecorder >>= \(sink) -> mkFilterPipeline sink [mkBuffer 50]
      in env environment $ \e -> bench "bufferBench50" $ nfIO $ do
          writeChan p e
    , let environment = mkNullRecorder >>= \(sink) -> mkFilterPipeline sink [mkBuffer 500]
      in env environment $ \e -> bench "bufferBench500" $ nfIO $ do
          writeChan p e

    ]

writeChan :: ( MonadIO m
             , ChanLike cit cot IstatdDatum
             )
          => Proxy (cit IstatdDatum)
          -> cit IstatdDatum
          -> m ()
writeChan _ e = clWriteChan e $ IstatdDatum Counter "CounterName" 0 1
