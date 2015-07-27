-- |
-- Moduleful of ways to "lift" and then run user-supplied parsers and
-- combinations of those. A signature of a runner is
-- ``Parsec String a (Entry, EntryAdder) -> Parsec String ParseState ()``
-- where @ParseState@, as defined in Types.hs, is
--``(Entry, EntryAdder, Space)``.
--
-- Runners can be thought of performing the following instructions —
-- "take a combination of user-supplied parsers and return a parser
-- that runs it against inputs, saving matches into the first and
-- second registries of ParseState, dumping previously stored data
-- into Space if needed".
--
-- We also provide a function to run an arbitrary runner.

module Scrollkeeper.Runners where

import           Data.Functor.Identity (runIdentity)
import           Data.Map              (empty)
import           Scrollkeeper.Extras
import           Scrollkeeper.Types
import           Text.Parsec

-- | Simply turns user-supplied parser into state-modifying parser.
liftp :: Parsec String ParseState (Entry, EntryAdder) -> Parsec String ParseState ()
liftp = liftpWith id

-- | Turns user-supplied parser into state-modifying parser, modifying
-- @Entry@ that is getting added to the @Space@.
liftpWith :: (Entry -> Entry) ->                             -- ^ Entry transformer
            Parsec String ParseState (Entry, EntryAdder) -> -- ^ User-supplied parser
            Parsec String ParseState ()                     -- ^ "Lifted" parser
liftpWith t x = do
  (e, f) <- x
  modifyState $ \(a, s) -> let f' = f e a
                           in (fst f', t (snd f') : s)

evalp :: Parsec String ParseState () -> Parsec String ParseState Space
evalp x = x >> getState >>= return . conc

-- | Runner for "lifted" parsers.
run :: Parsec String ParseState () -> String -> PE Space
run p x = runIdentity $ runParserT (evalp p) (empty, []) "" x
