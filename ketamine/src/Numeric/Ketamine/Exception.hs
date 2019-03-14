module Numeric.Ketamine.Exception (
    tryAll
  , tryEvaluate
  , EpisodeCompleted(..)
  ) where

import Control.Exception (evaluate)
import Control.Exception.Safe

--import Control.Monad.Catch (MonadCatch(..), throwM)
import System.IO.Unsafe (unsafePerformIO)


data EpisodeCompleted = EpisodeCompleted deriving (Show, Typeable)

instance Exception EpisodeCompleted

tryAll :: MonadCatch m => m a -> m (Either SomeException a)
tryAll m =
  catch (fmap Right m) $ \exception ->
    case fromException exception :: Maybe SomeAsyncException of
      Nothing ->
        pure $ Left exception
      Just async ->
        throwM async

tryEvaluate :: a -> Either SomeException a
tryEvaluate x = unsafePerformIO (tryAll (evaluate x))
