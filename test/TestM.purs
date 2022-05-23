module TestM where

import Prelude
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , throwError
  )
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Medea (ValidationError(EmptyError))
import Effect.Aff (Aff)
import Mote (MoteT)

type TestPlanM (a :: Type) = MoteT (Const Void) (Aff Unit) Aff a

-- this silly thing is needed because Medea's `validate` needs both
-- MonadPlus and MonadError, there must be a better way
-- or it should be upstreamed to medea-ps as a default
newtype ValidationM (a :: Type) = ValidationM
  (ExceptT ValidationError Identity a)

derive newtype instance Functor ValidationM
derive newtype instance Apply ValidationM
derive newtype instance Applicative ValidationM
derive newtype instance Bind ValidationM
derive newtype instance Monad ValidationM
derive newtype instance MonadThrow ValidationError ValidationM

derive newtype instance MonadError ValidationError ValidationM

-- note: MonadZero is being deprecated
derive newtype instance MonadZero ValidationM
derive newtype instance MonadPlus ValidationM
instance Alt ValidationM where
  alt (ValidationM first) (ValidationM second) = case runExceptT first of
    (Identity (Right a)) -> pure a
    (Identity (Left _)) -> case runExceptT second of
      (Identity (Right a)) -> pure a
      (Identity (Left e)) -> throwError e

instance Plus ValidationM where
  empty = throwError EmptyError

instance Alternative ValidationM

runValidationM :: forall (a :: Type). ValidationM a -> Either ValidationError a
runValidationM (ValidationM etvia) = do
  let (Identity eva) = runExceptT etvia
  eva
