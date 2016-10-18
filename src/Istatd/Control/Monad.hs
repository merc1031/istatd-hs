module Istatd.Control.Monad
( (<<)
)
where

import Control.Monad ((=<<))

(<<)
  :: (Monad m)
  => m b
  -> m ()
  -> m b
a << s = const a =<< s
