module MCMC.Types (module MCMC.Types,
                   module Numeric.LogDouble,
                   module Range)
    where

import Numeric.LogDouble
import Range

-- data ContextIndex = ContextIndex Int
type ContextIndex = Int

newtype ContextAction a =
    ContextAction { runContextAction :: ContextIndex -> IO a }

instance Functor ContextAction where
    fmap f action =
        ContextAction $ \context -> fmap f (runContextAction action context)

instance Applicative ContextAction where
    pure value = ContextAction $ \_ -> pure value

    -- Evaluate both applicative operands against the same context.
    function <*> action = ContextAction $ \context -> do
        f <- runContextAction function context
        value <- runContextAction action context
        pure (f value)

instance Monad ContextAction where
    -- Run both sides of a dependent calculation against the same context.
    action >>= next = ContextAction $ \context -> do
        value <- runContextAction action context
        runContextAction (next value) context

data TransitionKernel = TransitionKernel (ContextIndex -> IO ())

runTK c (TransitionKernel kernel) = kernel c

data Proposal = Proposal (ContextIndex -> IO LogDouble)

-- It is unfortunate that modifiable-ness is not visible at the type level.
type Modifiable a = a
