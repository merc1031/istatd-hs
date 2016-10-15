{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
import            Control.Monad.IO.Class
import            Control.Monad.Catch             ( MonadCatch )
import            Control.Monad                   ( void
                                                  , replicateM_
                                                  )
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
      let e = mkPipeEnv [mkFilterPrefix "HiImAPrefix"]
      in genStandardBenchGroup p "prefixBench" e
    , let e = mkPipeEnv [mkFilterPrefix "HiImAPrefixThatIsVeryVeryVeryVeryLongLongLong"]
      in genStandardBenchGroup p "prefixBenchLong" e
    , let e = mkPipeEnv [ mkFilterPrefix "Hi"
                        , mkFilterPrefix "Im"
                        , mkFilterPrefix "A"
                        , mkFilterPrefix "Prefix"
                        ]
      in genStandardBenchGroup p "prefixBench4" e
    ]

suffixBench :: ( NFData (cit IstatdDatum)
               , NFData (cot IstatdDatum)
               , ChanLike cit cot IstatdDatum
               )
            => Proxy (cit IstatdDatum)
            -> Benchmark
suffixBench p =
  bgroup "suffixBenches" [
      let e = mkPipeEnv [mkFilterSuffix "HiImASuffix"]
      in genStandardBenchGroup p "suffixBench" e
    , let e = mkPipeEnv [mkFilterSuffix "HiImASuffixThatIsVeryVeryVeryVeryLongLongLong"]
      in genStandardBenchGroup p "suffixBenchLong" e
    , let e = mkPipeEnv [ mkFilterSuffix "Hi"
                        , mkFilterSuffix "Im"
                        , mkFilterSuffix "A"
                        , mkFilterSuffix "Suffix"
                        ]
      in genStandardBenchGroup p "suffixBench4" e
    ]

prefixSuffixBench :: ( NFData (cit IstatdDatum)
                     , NFData (cot IstatdDatum)
                     , ChanLike cit cot IstatdDatum
                     )
                  => Proxy (cit IstatdDatum)
                  -> Benchmark
prefixSuffixBench p =
  bgroup "prefixSuffixBenches" [
      let e = mkPipeEnv [mkFilterSuffix "HiImASuffix"
                                  , mkFilterPrefix "HiImAPrefix"
                                  ]
      in genStandardBenchGroup p "prefixSuffixBench" e
    ]

bufferBench5 :: ( NFData (cit IstatdDatum)
                , NFData (cot IstatdDatum)
                , ChanLike cit cot IstatdDatum
                )
             => Proxy (cit IstatdDatum)
             -> Benchmark
bufferBench5 p =
  bgroup "bufferBenches" [
      let e = mkPipeEnv [mkBuffer 0]
      in genStandardBenchGroup p "bufferBench 0" e
    , let e = mkPipeEnv [mkBuffer 1]
      in genStandardBenchGroup p "bufferBench1" e
    , let e = mkPipeEnv [mkBuffer 5]
      in genStandardBenchGroup p "bufferBench5" e
    , let e = mkPipeEnv [mkBuffer 50]
      in genStandardBenchGroup p "bufferBench50" e
    , let e = mkPipeEnv [mkBuffer 500]
      in genStandardBenchGroup p "bufferBench500" e
    ]

data Bench = BenchSingle
           | BenchNumber Int

genStandardBenchGroup :: ( NFData (cit IstatdDatum)
                         , NFData (cot IstatdDatum)
                         , ChanLike cit cot IstatdDatum
                         )
                      => Proxy (cit IstatdDatum)
                      -> String
                      -> (IO (cot IstatdDatum, cit IstatdDatum))
                      -> Benchmark
genStandardBenchGroup p groupName environment =
       genBenchGroup p groupName environment [BenchSingle, BenchNumber 100, BenchNumber 500]

genBenchGroup :: ( NFData (cit IstatdDatum)
                 , NFData (cot IstatdDatum)
                 , ChanLike cit cot IstatdDatum
                 )
              => Proxy (cit IstatdDatum)
              -> String
              -> (IO (cot IstatdDatum, cit IstatdDatum))
              -> [Bench]
              -> Benchmark
genBenchGroup p groupName environment benchmarks =
       bgroup groupName $ map mkBench benchmarks
    where
        mkBench BenchSingle =
          env environment $ \e -> bench "singleWrite" $ nfIO $ do
            writeChanB p e
        mkBench (BenchNumber n) =
          env environment $ \e -> bench ("write" ++ show n) $ nfIO $ do
            writeChanNB p n e

mkPipeEnv :: ( MonadIO m
             , MonadCatch m
             , ChanLike cit cot IstatdDatum
             )
          => [FilterFunc (cit IstatdDatum) m]
          -> m (cot IstatdDatum, cit IstatdDatum)
mkPipeEnv ps = do
  (inC, outC) <- newZChan
  sink <- mkPipeRecorder inC
  fp <- mkFilterPipeline sink ps
  return (outC, fp)

writeChanNB :: ( MonadIO m
              , ChanLike cit cot IstatdDatum
              )
           => Proxy (cit IstatdDatum)
           -> Int
           -> (cot IstatdDatum, cit IstatdDatum)
           -> m ()
writeChanNB p n (c, e) = do
  writeChanNU p n e
  void $ liftIO $ replicateM_ n $ readChan c

writeChanNU :: ( MonadIO m
              , ChanLike cit cot IstatdDatum
              )
           => Proxy (cit IstatdDatum)
           -> Int
           -> (cit IstatdDatum)
           -> m ()
writeChanNU _ n e = do
  replicateM_ n $ writeChan e $ IstatdDatum Counter "CounterName" 0 1

writeChanB :: ( MonadIO m
              , ChanLike cit cot IstatdDatum
              )
           => Proxy (cit IstatdDatum)
           -> (cot IstatdDatum, cit IstatdDatum)
           -> m ()
writeChanB p (c, e) = do
  writeChanU p e
  void $ liftIO $ readChan c

writeChanU :: ( MonadIO m
              , ChanLike cit cot IstatdDatum
              )
           => Proxy (cit IstatdDatum)
           -> (cit IstatdDatum)
           -> m ()
writeChanU _ e = do
  writeChan e $ IstatdDatum Counter "CounterName" 0 1
