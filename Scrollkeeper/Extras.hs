-- |
-- Functions that didn't qualify to fit other modules reside here.

module Scrollkeeper.Extras where

import           Scrollkeeper.Types

-- | EntryAdder combinator.
(.>.) :: EntryAdder -> EntryAdder -> EntryAdder
(f .>. g) old new = g (fst f') (snd f')
  where
    f' = f old new

-- | Concatenate tuple into a list with head = fst, tail = snd
conc :: (a, [a]) -> [a]
conc x = (fst x) : (snd x)
