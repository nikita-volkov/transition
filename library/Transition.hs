module Transition
(
  Transition(..),
  TransitionResult(..),
  transitionTVar,
  liftState,
)
where

import Transition.Prelude


{-|
Like 'State', but also communicates the information on whether
the state value has been updated.
This comes in especially handy when applying it to the content of 'TVar',
since it lets us avoid redundant writes and hence reduces the contention.
-}
newtype Transition s a = Transition (s -> TransitionResult s a)

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
Access the contents of tvar and optionally modify them.
-}
transitionTVar :: TVar s -> Transition s a -> STM a
transitionTVar tv (Transition update) = do
  s <- readTVar tv
  case update s of
    ChangedTransitionResult a newS -> writeTVar tv newS $> a
    UnchangedTransitionResult a -> return a

{-|
Lift a 'State' computation by checking whether its underlying value changes using 'Eq'.
-}
liftState :: Eq s => State s a -> Transition s a
liftState m = Transition $ \ s -> case runState m s of
  (a, newS) -> if newS == s
    then UnchangedTransitionResult a
    else ChangedTransitionResult a newS
