{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase, DeriveFunctor, RankNTypes #-}
{-# LANGUAGE TypeApplications, TemplateHaskell , ExistentialQuantification           #-}

 {-# OPTIONS_GHC -w #-}
-- | Environment values with stateful capabilities.
module Numeric.Ketamine.Effect.Ref where


import Numeric.Ketamine.Types
--import Control.Lens (Lens', lens, view, iview)
--import Control.Lens.At (At(..), Index, IxValue)
import Control.Applicative (Const(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Void (Void)
--import UnliftIO
import Data.Functor.Identity

--import qualified Control.Lens as Lens
--import qualified Control.Lens as L
--import qualified Control.Lens.Internal.Bazaar as L
--import qualified Control.Lens.Internal.Context as L

--import Control.Lens.Type
--import Control.Lens.Internal.Prism (Market, Market')
import Data.IORef
import Data.Tuple (swap)
import Data.Either (either)

--These can be the same IORef when you want to be modifying one reference.
--could extend to STRefs, MVars and TVars:
--http://hackage.haskell.org/package/global-variables-1.0.1.1/docs/Data-Global.html
--data Ref a b = forall s t. Ref (IORef s) (IORef t) (Lens s t a b)
--data Ref' i a b = forall s t. Ref' (IORef s) (IORef t) (IndexedLens i s t a b)
--mapl :: Lens' a b -> Ref a -> Ref b
--maplike :: ((a1 -> f a1) -> a2 -> f a2) -> Ref f a2 -> Ref f a1
{-

data Ref1 x f a = forall s . Ref1 (x s) (LensLike f s s a a)
type RRef1 x a = Ref1 x (Const a) a
type WRef1 x a = Ref1 x Identity a

type IOWRef1 a = WRef1 IORef a
--type LRef1 r a = forall f . Functor f => Ref1 r f a
type IORRef1 a = RRef1 IORef a
--LensLike (Const a) a a a a

--type STRef1 = Ref1 STRef
--type MVar1 = Ref1 MVar
maplike1 :: LensLike f a1 a1 a2 a2 -> Ref1 r f a1 -> Ref1 r f a2
maplike1 l = f $ traverseOf l
  where f l' (Ref1 ref l) = Ref1 ref (l . l')

instance Contravariant f => Functor (Ref1 r f) where
    fmap = maplike1 . to

maplike2 :: LensLike f a1 b1 a2 b2 -> Ref2 r f a1 b1 -> Ref2 r f a2 b2
maplike2 l = f $ traverseOf l
  where f l' (Ref2 r1 r2 l) = Ref2 r1 r2 (l . l')

--TODO grok behavior
modifyRef1' :: Ref1 IORef ((,) b) a -> (a -> (a, b)) -> IO b
modifyRef1' (Ref1 r l) f = atomicModifyIORef' r (swap . traverseOf l (swap . f))

modifyRef1 :: IOWRef1 a -> (a -> a) -> IO ()
modifyRef1 (Ref1 r l) f = modifyIORef r (over l f)

--r <- newIORRef1 1 (to (+1))
--n <- newIOWRef1 (1,2) _1
--n <- newIOLRef1 (1,2) _1
--
--sayref (Ref1 r _) = readIORef r

readIORef1 :: RRef1 IORef a -> IO a
readIORef1 (Ref1 r l) = view l <$> readIORef r

newIORef1' :: r -> IO (Ref1 IORef f r)
newIORef1' r = newIORef1 r id

--newRRef1 :: r -> IO (RRef1 IORef a)
newIORef1 :: s -> LensLike f s s a a -> IO (Ref1 IORef f a)
newIORef1 r g = newIORef r >>= \r' -> return (Ref1 r' g) 

--r <- newIORRef1 1 (to (+1))
newIORRef1 :: s -> LensLike (Const a) s s a a -> IO (RRef1 IORef a)
newIORRef1 = newIORef1

-- n <- newIOWRef1 (1,2) _1
newIOWRef1 :: s -> LensLike Identity s s a a -> IO (WRef1 IORef a)
newIOWRef1 = newIORef1
-}

--data Ref x p q f a b = forall s t . Ref (Optical p q f s t a b) (x s) (x t)
--data Ref' x p q f a = forall s . Ref' (Optical' p q f s a) (x s)

data Ref x f a b = forall s t . Ref (LensLike f s t a b) (x s) (x t)

newRef :: s -> t -> LensLike f s t a b -> IO (Ref IORef f a b)
newRef s t o = (Ref o) <$> newIORef s <*> newIORef t 

newRef' :: s -> LensLike f s s a b -> IO (Ref IORef f a b)
newRef' s o = do
    s' <- newIORef s 
    return $ Ref o s' s'

withSetter :: LensLike Identity s t a b -> (((a -> b) -> s -> t) -> r) -> r
withSetter l f = f . cloneSetter $ l

cloneSetter :: LensLike Identity s t a b -> ((a -> b) -> s -> t)
cloneSetter l afb = runIdentity . l (Identity . afb)

withGetter :: LensLike (Const c) s t a b -> (((a -> c) -> s -> c) -> r) -> r
withGetter l f = f . cloneGetter $ l

cloneGetter :: LensLike (Const c) s t a b -> (a -> c) -> s -> c
cloneGetter l afb = getConst . l (Const . afb)

viewRef :: Ref IORef (Const a) a b -> IO a
viewRef (Ref l s t) = with <$> readIORef s  
    where with s' = withGetter l $ \acsc -> acsc id s' 

overRef :: Ref IORef Identity a b -> (a -> b) -> IO ()
overRef (Ref l s t) f = with =<< readIORef s  
    where with s' = withSetter l $ \abst -> (writeIORef t $ abst f s')



--newLensRef :: PrimMonad m => s -> Lens s s a a -> m (RRef1 (PrimState m) a)
{-
http://hackage.haskell.org/package/primitive-0.6.4.0/docs/Data-Primitive-MutVar.html
also MVar, TVar, Ptr


s = "5"
s' <- newIORef s
o' = Ref _Show s' s'
>  _ShowInt = _Show :: Prism' String Int
> o' = Ref _ShowInt s' s'
modifyP o' (+1)
> readIORef s'
"6"

s = ("hi!",2) :: (String, Int)
t = (4,2)  :: (Int, Int)

s' <- newIORef s
t' <- newIORef t
o = Ref _1 s' t'
o' = Ref _1 s' s'
modifyL o' tail
readIORef s'
> readIORef s'
("i!",2)
> readIORef t'
(4,2)
> modifyL o length
> readIORef s'
("i!",2)
> readIORef t'
(2,2)

import Data.Monoid

s = ["hi", " there"] :: [String]
t = fmap Sum [1..10] :: [Sum Int]

s' <- newIORef s
t' <- newIORef t

o = Ref traverse s' t'
o' = Ref traverse s' s'

o :: Applicative f => Ref IORef (->) (->) f String (Sum Int)
o = Ref traverse s' t'
o' :: Applicative f => Ref IORef (->) (->) f String String
o' = Ref traverse s' s'

> y <- view o
> y
"hi there"

over o (Sum . length)
> readIORef t'
[Sum {getSum = 2},Sum {getSum = 6}]


-}



{-
--type Getting r s a = (a -> Const r a) -> s -> Const r s

-- | Abstraction over how to read from and write to a mutable rference
--
data Ref f a b = forall r . Ref (IORef r) (LensLike f r r a b)


type Ref' f a = Ref f a a
type LensRef a b = forall f . Functor f => Ref f a b
type LensRef' a = LensRef a a

type RRef a = Ref (Const a) a a
type WRef a b = Ref Identity a b
type MRef a b r = Ref ((,) r) a b


maplike :: LensLike f a1 b1 a2 b2 -> Ref f a1 b1 -> Ref f a2 b2
maplike l = f $ traverseOf l
  where f l' (Ref ref l) = Ref ref (l . l')

--modifyRef :: Ref ((,) r) a -> (a -> (a, r)) -> IO r
--ito :: (Indexable i p, Contravariant f) => (s -> (i, a)) -> Over' p f s a
ito' :: (Indexable i p, Contravariant f) => (s -> (a, i)) -> Over' p f s a
ito' = ito . (swap .)

foo :: Contravariant f => (b1 -> (a2, i)) -> Ref f b1 b1 -> Ref f a2 a2
foo = maplike . ito'

-- TODO make Ref an inst of Contravariant?
-- instance Contravariant f => Functor (Ref' f) where
bar :: Contravariant f => (a -> b) -> Ref' f a -> Ref' f b
bar = maplike . to

modifyRef :: MRef a b r -> (a -> (b, r)) -> IO r
modifyRef (Ref r l) f = atomicModifyIORef' r (swap . traverseOf l (swap . f))

newRef :: r -> IO (Ref f r r)
newRef r = newIORef r >>= \r' -> return (Ref r' id)

readRef :: RRef a -> IO a
readRef (Ref r l) = view l <$> readIORef r

writeRef :: WRef a b -> b -> IO ()
writeRef (Ref r l) a = atomicModifyIORef_ r (set l a)

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = atomicModifyIORef' ref (\a -> (f a, ()))

viewsRef
  :: (Conf a r, MonadReader r m, MonadIO m)
  => (a -> RRef b) -> m b
viewsRef f = views conf f >>= liftIO . readRef

viewRefWith
  :: (Conf r1 r2, MonadReader r2 m, MonadIO m) => Getting (RRef b) r1 (RRef b) -> m b
viewRefWith l = view (conf . l) >>= liftIO . readRef

viewRef :: (Conf (RRef b) r, MonadReader r m, MonadIO m) => m b
viewRef = viewRefWith id

viewRefAt
  :: Conf (Ref (Const (Maybe (IxValue a))) a a) r
  => MonadReader r m
  => MonadIO m
  => At a
  => Index a -> m (Maybe (IxValue a))
viewRefAt i = viewsRef $ maplike (at i)

ixRef :: (Ixed a, Applicative f) => Index a -> Ref' f a -> Ref' f (IxValue a)
ixRef = maplike . ix

atRef :: (At a, Functor f) => Index a -> Ref' f a -> Ref' f (Maybe (IxValue a))
atRef = maplike . at

-}

{-
viewRefIxed
  :: Conf (Ref (Const (IxValue a)) a a) r
  => MonadReader r m
  => MonadIO m
  => Ixed a
  => Monoid (IxValue a) -- TODO ?
  => Index a -> m (IxValue a)
viewRefIxed i = viewsRef $ maplike (ix i)
-}
--overRef f = views conf f >>= liftIO . readRef


