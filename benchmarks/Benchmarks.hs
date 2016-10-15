{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
import            Control.Monad.IO.Class
import            Control.Monad.Catch             ( MonadCatch )
import            Control.Monad                   ( void )
import            Control.DeepSeq                 ( NFData )
import            Criterion.Main
import            Criterion.Types
import            Data.Proxy
import            Istatd.Istatd

import qualified  Istatd.Chan.ChanT               as ChanT
import qualified  Istatd.Chan.Chan                as Chan

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
               , NFData (cot IstatdDatum)
               , ChanLike cit cot IstatdDatum
               )
            => Proxy (cit IstatdDatum)
            -> Benchmark
prefixBench p =
  bgroup "prefixBenches" [
      let environment = mkPipeEnv [mkFilterPrefix "HiImAPrefix"]
      in env environment $ \e -> bench "prefixBench" $ nfIO $ do
          writeChan p e
    , let environment = mkPipeEnv [mkFilterPrefix "HiImAPrefixThatIsVeryVeryVeryVeryLongLongLong"]
      in env environment $ \e -> bench "prefixBenchLong" $ nfIO $ do
          writeChan p e
    , let environment = mkPipeEnv [ mkFilterPrefix "Hi"
                                  , mkFilterPrefix "Im"
                                  , mkFilterPrefix "A"
                                  , mkFilterPrefix "Prefix"
                                  ]
      in env environment $ \e -> bench "prefixBench4" $ nfIO $ do
          writeChan p e
    ]

suffixBench :: ( NFData (cit IstatdDatum)
               , NFData (cot IstatdDatum)
               , ChanLike cit cot IstatdDatum
               )
            => Proxy (cit IstatdDatum)
            -> Benchmark
suffixBench p =
  bgroup "suffixBenches" [
      let environment = mkPipeEnv [mkFilterSuffix "HiImASuffix"]
      in env environment $ \e -> bench "suffixBench" $ nfIO $ do
          writeChan p e

    , let environment = mkPipeEnv [mkFilterSuffix "HiImASuffixThatIsVeryVeryVeryVeryLongLongLong"]
      in env environment $ \e -> bench "suffixBenchLong" $ nfIO $ do
          writeChan p e
    , let environment = mkPipeEnv [ mkFilterSuffix "Hi"
                                  , mkFilterSuffix "Im"
                                  , mkFilterSuffix "A"
                                  , mkFilterSuffix "Suffix"
                                  ]
      in env environment $ \e -> bench "suffixBench4" $ nfIO $ do
          writeChan p e
    ]

prefixSuffixBench :: ( NFData (cit IstatdDatum)
                     , NFData (cot IstatdDatum)
                     , ChanLike cit cot IstatdDatum
                     )
                  => Proxy (cit IstatdDatum)
                  -> Benchmark
prefixSuffixBench p =
  bgroup "prefixSuffixBenches" [
      let environment = mkPipeEnv [mkFilterSuffix "HiImASuffix"
                                  , mkFilterPrefix "HiImAPrefix"
                                  ]
      in env environment $ \e -> bench "prefixSuffixBench" $ nfIO $ do
          writeChan p e

    ]

bufferBench5 :: ( NFData (cit IstatdDatum)
                , NFData (cot IstatdDatum)
                , ChanLike cit cot IstatdDatum
                )
             => Proxy (cit IstatdDatum)
             -> Benchmark
bufferBench5 p =
  bgroup "bufferBenches" [
      let environment = mkPipeEnv [mkBuffer 0]
      in env environment $ \e -> bench "bufferBench0" $ nfIO $ do
          writeChan p e
    , let environment = mkPipeEnv [mkBuffer 1]
      in env environment $ \e -> bench "bufferBench1" $ nfIO $ do
          writeChan p e
    , let environment = mkPipeEnv [mkBuffer 5]
      in env environment $ \e -> bench "bufferBench5" $ nfIO $ do
          writeChan p e
    , let environment = mkPipeEnv [mkBuffer 50]
      in env environment $ \e -> bench "bufferBench50" $ nfIO $ do
          writeChan p e
    , let environment = mkPipeEnv [mkBuffer 500]
      in env environment $ \e -> bench "bufferBench500" $ nfIO $ do
          writeChan p e

    ]

mkPipeEnv :: ( MonadIO m
             , MonadCatch m
             , ChanLike cit cot IstatdDatum
             )
          => [FilterFunc (cit IstatdDatum) m]
          -> m (cot IstatdDatum, cit IstatdDatum)
mkPipeEnv ps = do
  (inC, outC) <- clNewZChan
  sink <- mkPipeRecorder inC
  fp <- mkFilterPipeline sink ps
  return (outC, fp)

writeChan :: ( MonadIO m
             , ChanLike cit cot IstatdDatum
             )
          => Proxy (cit IstatdDatum)
          -> (cot IstatdDatum, cit IstatdDatum)
          -> m ()
writeChan _ (c, e) = do
  clWriteChan e $ IstatdDatum Counter "CounterName" 0 1
  void $ liftIO $ clReadChan c
