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
import Control.Applicative (Const(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Functor.Identity
import Data.Tuple (swap)

import Control.Monad.Primitive
import Data.Primitive.MutVar (MutVar)
import qualified Data.Primitive.MutVar as M


{-

s = ["hi", "there"] :: [String]

s' <- M.newMutVar s
l = Ref traverse s'
> f a = (a ++ a, a)
> atomicModifyRef' l f
"hithere"
> readRef l
"hihitherethere"
> M.readMutVar s'
["hihi","therethere"]
> g a = (a,"bar")
> atomicModifyRef' l g
"barbar"
> M.readMutVar s'
["hihi","therethere"]
-}

-- | Abstraction over a mutable reference. 
data Ref x f a = forall s . Ref (LensLike' f s a) (MutVar x s)

instance Functor (Ref x (Const r)) where fmap = llmap . to

type IORef = Ref RealWorld
type IORead a = IORef (Const a) a
type IOWrite a = IORef Identity a

type STRef s = Ref s
type STRead s a = STRef s (Const a) a
type STWrite s a = STRef s Identity a

newRef :: PrimMonad m => s -> LensLike' f s a -> m (Ref (PrimState m) f a)
newRef s l = (Ref l) <$> M.newMutVar s 

readRef :: PrimMonad m => Ref (PrimState m) (Const a) a -> m a
readRef (Ref l r) = view l <$> M.readMutVar r

writeRef :: PrimMonad m => Ref (PrimState m) Identity a -> a -> m ()
writeRef (Ref l r) a = M.modifyMutVar r (set l a)

modifyRef :: PrimMonad m => Ref (PrimState m) Identity a -> (a -> a) -> m ()
modifyRef (Ref l r) f = M.modifyMutVar r (over l f)

atomicModifyRef
  :: PrimMonad m => Ref (PrimState m) ((,) b) a -> (a -> (a, b)) -> m b
atomicModifyRef (Ref l r) f = M.atomicModifyMutVar r (swap . traverseOf l (swap . f))

-- | Strict version of 'writeRef'.
writeRef' :: PrimMonad m => Ref (PrimState m) Identity a -> a -> m ()
writeRef' (Ref l r) a = M.modifyMutVar' r (set l a)

-- | Strict version of 'modifyRef'.
modifyRef' :: PrimMonad m => Ref (PrimState m) Identity a -> (a -> a) -> m ()
modifyRef' (Ref l r) f = M.modifyMutVar' r (over l f)

-- | Strict version of 'atomicModifyRef'.
atomicModifyRef'
  :: PrimMonad m => Ref (PrimState m) ((,) b) a -> (a -> (a, b)) -> m b
atomicModifyRef' (Ref l r) f = M.atomicModifyMutVar' r (swap . traverseOf l (swap . f))

viewRefWith
  :: (Conf r s, MonadReader s m, MonadIO m) 
  => Getting (IORead a) r (IORead a) -> m a
viewRefWith l = view (conf . l) >>= liftIO . readRef

viewRef 
  :: (Conf (IORead a) s, MonadReader s m, MonadIO m) 
  => m a
viewRef = viewRefWith id

viewsRef
  :: (MonadReader r m, Conf s r, MonadIO m) 
  => (s -> IORead a) -> m a
viewsRef f = views conf f >>= liftIO . readRef

llmap :: LensLike' f s a -> Ref x f s -> Ref x f a
llmap l = f $ traverseOf l where f l' (Ref l s) = Ref (l . l') s

ixRef :: (Ixed a, Applicative f) => Index a -> Ref x f a -> Ref x f (IxValue a)
ixRef = llmap . ix

atRef :: (At a, Functor f) => Index a -> Ref x f a -> Ref x f (Maybe (IxValue a))
atRef = llmap . at




{-

s = "5"
s' <- newMutVar s
o' = Ref2 _Show s' s'
>  _ShowInt = _Show :: Prism' String Int
> o' = Ref2 _ShowInt s' s'
modifyP o' (+1)
> readMutVar s'
"6"

s = ("hi!",2) :: (String, Int)
t = (4,2)  :: (Int, Int)

s' <- newMutVar s
t' <- newMutVar t
o = Ref2 _1 s' t'
o' = Ref2 _1 s' s'
modifyL o' tail
readMutVar s'
> readMutVar s'
("i!",2)
> readMutVar t'
(4,2)
> modifyL o length
> readMutVar s'
("i!",2)
> readMutVar t'
(2,2)

import Data.Monoid

s = ["hi", "there"] :: [String]
t = fmap Sum [1..10] :: [Sum Int]

s' <- M.newMutVar s
t' <- M.newMutVar t

o = Ref2 traverse s' t'
o' = Ref2 traverse s' s'

o :: Applicative f => Ref2 MutVar (->) (->) f String (Sum Int)
o = Ref2 traverse s' t'
o' :: Applicative f => Ref2 MutVar (->) (->) f String String
o' = Ref2 traverse s' s'

> y <- view o
> y
"hithere"

modifyRef2 o (Sum . length)
M.readMutVar t'
[Sum {getSum = 2},Sum {getSum = 6}]

modifyRef2 o' ("oh"++)
M.readMutVar s'
> M.readMutVar s'
["ohhi","ohthere"]

-}

-- | Abstraction over a tuple of mutable references. The first reference is read-only.
-- Use 'newRef2'' to mutate the first reference with a 'LensLike''.
data Ref2 x f a b = forall s t . Ref2 (LensLike f s t a b) (MutVar x s) (MutVar x t)

type IORef2 = Ref2 RealWorld
type IORead2 a b = IORef2 (Const a) a b
type IORead2' a = IORead2 a a
type IOWrite2 a b = IORef2 Identity a b
type IOWrite2' a = IOWrite2 a a

type STRef2 s = Ref2 s
type STRead2 s a b = STRef2 s (Const a) a b
type STRead2' s a = STRead2 s a a
type STWrite2 s a b = STRef2 s Identity a b
type STWrite2' s a = STWrite2 s a a


--data Ref2 x a b = forall s t . Ref2 (Lens s t a b) (MutVar x s) (MutVar x t)
--type LensRef2 x a b = forall f . Functor f => Ref2 x f a b

newRef2 :: PrimMonad m => s -> t -> LensLike f s t a b -> m (Ref2 (PrimState m) f a b)
newRef2 s t o = (Ref2 o) <$> M.newMutVar s <*> M.newMutVar t 

newRef2' :: PrimMonad m => s -> LensLike' f s a -> m (Ref2 (PrimState m) f a a)
newRef2' s o = do
    s' <- M.newMutVar s 
    return $ Ref2 o s' s'

readRef2 :: PrimMonad m => Ref2 (PrimState m) (Const a) a b -> m a
readRef2 (Ref2 l s _) = with <$> M.readMutVar s  
    where with s' = withGetter l $ \acsc -> acsc id s' 

modifyRef2 :: PrimMonad m => Ref2 (PrimState m) Identity a b -> (a -> b) -> m ()
modifyRef2 (Ref2 l s t) f = with =<< M.readMutVar s  
    where with s' = withSetter l $ \abst -> (M.writeMutVar t $! abst f s')


{-
atomicModifyRef2 :: PrimMonad m => Ref2 (PrimState m) a b -> (a -> (a, b)) -> m b
atomicModifyRef2 (Ref2 l s t) f = with =<< M.readMutVar s 
    where with s' = withLens l $ \sa sbt -> do
                        let (a, b) = f . sa $! s'
                        M.writeMutVar t $! sbt s' b (f . sa $! s')
                        return b

--ASetter s s a b -> Ref2 IORef2 s -> (a -> b) -> IO ()
--modifyL :: Ref2 IORef2 p q f a b1 -> (b2 -> t) -> IO b3
--modifyL :: Ref2 IORef2 (->) (->) Identity a b -> (a -> b) -> IO ()
--modifyL (Ref2 l s t) f = foo =<< readIORef2 s  where foo s' = withLens l $ \sa sbt -> (writeIORef2 t $ sbt s' (f . sa $ s'))

--withLens :: Lens s t a b -> ((s -> a) -> (s -> b -> t) -> r) -> r
withLens = undefined
-}

--  :: (Conf r1 r2, MonadReader r2 m, MonadIO m) => Getting (IORef2 (Const b1) b1 b2) a (IORef2 (Const b1) b1 b2) -> m b1

viewRef2With
  :: (Conf r s, MonadReader s m, MonadIO m) 
  => Getting (IORead2 a b) r (IORead2 a b) -> m a
viewRef2With l = view (conf . l) >>= liftIO . readRef2

viewRef2 
  :: (Conf (IORead2 a b) s, MonadReader s m, MonadIO m) 
  => m a
viewRef2 = viewRef2With id

viewsRef2
  :: (MonadReader r m, Conf s r, MonadIO m)
  => (s -> IORead2 a b) -> m a
viewsRef2 f = views conf f >>= liftIO . readRef2

llmap2 :: LensLike f s t a b -> Ref2 x f s t -> Ref2 x f a b
llmap2 l = f $ traverseOf l where f l' (Ref2 l s t) = Ref2 (l . l') s t

mapRef2 :: (t -> b) -> Ref2 x (Const r) t t -> Ref2 x (Const r) b b 
mapRef2 = llmap2 . to

ixRef2 :: (Ixed a, Applicative f) => Index a -> Ref2 x f a a -> Ref2 x f (IxValue a) (IxValue a)
ixRef2 = llmap2 . ix

atRef2 :: (At a, Functor f) => Index a -> Ref2 x f a a -> Ref2 x f (Maybe (IxValue a)) (Maybe (IxValue a))
atRef2 = llmap2 . at

withSetter :: LensLike Identity s t a b -> (((a -> b) -> s -> t) -> r) -> r
withSetter l f = f . cloneSetter $ l

cloneSetter :: LensLike Identity s t a b -> ((a -> b) -> s -> t)
cloneSetter l afb = runIdentity . l (Identity . afb)

withGetter :: LensLike (Const c) s t a b -> (((a -> c) -> s -> c) -> r) -> r
withGetter l f = f . cloneGetter $ l

cloneGetter :: LensLike (Const c) s t a b -> (a -> c) -> s -> c
cloneGetter l afb = getConst . l (Const . afb)




