module Utils.Ffi
  ( MaybeFfiHelper
  , maybeFfiHelper
  , ContainerHelper
  , containerHelper
  , ErrorFfiHelper
  , errorHelper
  ) where

import Prelude

import Data.Either (Either(Left, Right), hush)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Data.Variant (Variant)
import Types.Error (E)

type MaybeFfiHelper =
  { nothing :: forall (x :: Type). Maybe x
  , just :: forall (x :: Type). x -> Maybe x
  , from :: forall (x :: Type). x -> Maybe x -> x
  }

type ErrorFfiHelper r =
  { error :: forall (x :: Type). String -> E r x
  , valid :: forall (x :: Type). x -> E r x
  , from :: forall (x :: Type). x -> E r x -> x
  }

errorHelper :: forall (v :: Row Type). (String -> Variant v) -> ErrorFfiHelper v
errorHelper v =
  { error: Left <<< v, valid: Right, from: \e -> hush >>> fromMaybe e }

maybeFfiHelper :: MaybeFfiHelper
maybeFfiHelper = { nothing: Nothing, just: Just, from: fromMaybe }

foreign import data ContainerHelper :: Type

foreign import _containerHelper
  :: { untuple :: forall (a :: Type). Tuple a a -> Array a
     , tuple :: forall (a :: Type) (b :: Type). a -> b -> Tuple a b
     }
  -> ContainerHelper

containerHelper :: ContainerHelper
containerHelper = _containerHelper { untuple, tuple: Tuple }

untuple :: forall (a :: Type). Tuple a a -> Array a
untuple (Tuple a b) = [ a, b ]
