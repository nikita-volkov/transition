module Transition
(
  Transition(..),
  TransitionResult(..),
  liftState,
  transitionTVar,
  transitionMapValue,
)
where

import Transition.Prelude
import qualified Data.Map.Strict as Map


{-|
Like 'State', but also communicates the information on whether
the state value has been updated.
This comes in especially handy when applying it to the content of 'TVar',
since it lets us avoid redundant writes and hence reduces the contention.
-}
newtype Transition s a =
  {-|
  You can think of it as the following function:

  @s -> (a, Maybe s)@

  The 'TransitionResult' type just lets us represent the same thing more efficiently,
  and lets us control the strictness to avoid space leaks,
  which is a problem of the 'State' monad.
  -}
  Transition (s -> TransitionResult s a)

data TransitionResult s a = UnchangedTransitionResult ~a | ChangedTransitionResult ~a !s

deriving instance Functor (TransitionResult s)

deriving instance Functor (Transition s)

instance Applicative (Transition s) where
  pure a = Transition (const (UnchangedTransitionResult a))
  (<*>) (Transition lf) (Transition rf) = Transition $ \ s ->
    case lf s of
      ChangedTransitionResult la newS -> fmap la (rf newS)
      UnchangedTransitionResult la -> fmap la (rf s)

instance Monad (Transition s) where
  return = pure
  (>>=) (Transition lf) rk = Transition $ \ s ->
    case lf s of
      ChangedTransitionResult la newS -> case rk la of Transition rf -> rf newS
      UnchangedTransitionResult la -> case rk la of Transition rf -> rf s

{-|
Lift a 'State' computation by checking whether its underlying value changes using 'Eq'.
-}
liftState :: Eq s => State s a -> Transition s a
liftState m = Transition $ \ s -> case runState m s of
  (a, newS) -> if newS == s
    then UnchangedTransitionResult a
    else ChangedTransitionResult a newS

{-|
Access the contents of tvar and optionally modify them.
-}
transitionTVar :: TVar s -> Transition s a -> STM a
transitionTVar tv (Transition update) = do
  s <- readTVar tv
  case update s of
    ChangedTransitionResult a newS -> writeTVar tv newS $> a
    UnchangedTransitionResult a -> return a

transitionMapValue :: Ord k => k -> Transition (Maybe v) a -> Transition (Map.Map k v) a
transitionMapValue k (Transition valueTransitionFn) =
  Transition $ \ map ->
    case Map.alterF alterFn k map of
      (newMapFn, newMap) -> newMapFn newMap
  where
    alterFn maybeVal =
      case valueTransitionFn maybeVal of
        ChangedTransitionResult a newMaybeValue ->
          (ChangedTransitionResult a, newMaybeValue)
        UnchangedTransitionResult a ->
          (const (UnchangedTransitionResult a), maybeVal)
