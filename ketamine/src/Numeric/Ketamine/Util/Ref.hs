{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE RankNTypes #-}


{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE ImpredicativeTypes #-}

 {-# OPTIONS_GHC -w #-}
-- | Environment values with stateful capabilities.
module Numeric.Ketamine.Util.Ref where


import Numeric.Ketamine.Types
import Control.Applicative (Const(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Primitive (PrimMonad, PrimState, RealWorld)
import Control.Monad.Reader (MonadReader)
import Data.Functor.Identity
import Data.Tuple (swap)


import Data.Primitive.MutVar (MutVar)
import qualified Data.Primitive.MutVar as M

import Data.Bitraversable
import GHC.Exts (Constraint)
import qualified Control.Lens as L
import qualified Control.Lens.Internal.Prism as L

{-
import Lens.Micro.GHC

im = IntMap.fromList [(1,"hi"), (2,"there")]
im' <- M.newMutVar im

r = Ref id im' 
r' = ixRef 1 r
r'' = atRef 1 r

> readRef r'
"hi"
> readRef r''
Just "hi"

rbad' = ixRef 3 r

> readRef rbad'
""

ixRef :: (Ixed a, Applicative f) => Index a -> Ref x f a -> Ref x f (IxValue a)
ixRef = llmap . ix


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

data Ref x a = forall s . Ref (Lens' s a) (MutVar x s)

type IORef = Ref RealWorld
type STRef s = Ref s

newRef :: PrimMonad m => MutVar (PrimState m) s -> Lens' s a -> Ref (PrimState m) a
newRef s l = Ref l s

-- newGlobalRef --use logger-simple trick
-- newLocalRef 

readRef :: PrimMonad m => Ref (PrimState m) a -> m a
readRef (Ref l r) = view l <$> M.readMutVar r

writeRef :: PrimMonad m => Ref (PrimState m) a -> a -> m ()
writeRef (Ref l r) a = M.modifyMutVar r (set l a)

modifyRef :: PrimMonad m => Ref (PrimState m) a -> (a -> a) -> m ()
modifyRef (Ref l r) f = M.modifyMutVar r (over l f)

atomicModifyRef
  :: PrimMonad m => Ref (PrimState m) a -> (a -> (a, b)) -> m b
atomicModifyRef (Ref l r) f = M.atomicModifyMutVar r (swap . traverseOf l (swap . f))

-- | Strict version of 'writeRef'.
writeRef' :: PrimMonad m => Ref (PrimState m) a -> a -> m ()
writeRef' (Ref l r) a = M.modifyMutVar' r (set l a)

-- | Strict version of 'modifyRef'.
modifyRef' :: PrimMonad m => Ref (PrimState m) a -> (a -> a) -> m ()
modifyRef' (Ref l r) f = M.modifyMutVar' r (over l f)

-- | Strict version of 'atomicModifyRef'.
atomicModifyRef'
  :: PrimMonad m => Ref (PrimState m) a -> (a -> (a, b)) -> m b
atomicModifyRef' (Ref l r) f = M.atomicModifyMutVar' r (swap . traverseOf l (swap . f))

llmap :: Lens' s a -> Ref x s -> Ref x a
llmap l' (Ref l r) = Ref (l . l') r

viewRefWith
  :: (Conf r s, MonadReader s m, MonadIO m) 
  => Getting (IORef a) r (IORef a) -> m a
viewRefWith l = view (conf . l) >>= liftIO . readRef

viewRef 
  :: (Conf (IORef a) s, MonadReader s m, MonadIO m) 
  => m a
viewRef = viewRefWith id

viewsRef
  :: (MonadReader r m, Conf s r, MonadIO m) 
  => (s -> IORef a) -> m a
viewsRef f = views conf f >>= liftIO . readRef


{-

s = "5"
s' <- M.newMutVar s
o' = newPRef s' s' L._Show 
modifyPRef o' (+1)
M.readMutVar s'
"6"
-}


{-
s = ("hi!",2) :: (String, Int)
t = (4,2)  :: (Int, Int)

s' <- M.newMutVar s
t' <- M.newMutVar t
o = newLRef s' t' _1
o' = newLRef s' s' _1

modifyLRef o' tail
M.readMutVar s'
-- ("i!",2)
M.readMutVar t'
--(4,2)
--
modifyLRef o length
M.readMutVar s'
-- ("i!",2)
M.readMutVar t'
-- (2,2)
-}

{-
import Data.Monoid

s = ["hi", "there"] :: [String]
t = fmap Sum [1..10] :: [Sum Int]

s' <- M.newMutVar s
t' <- M.newMutVar t

o = newLRef s' t' traverse 
o' = newLRef s' s' traverse 

readLRef o
--"hithere"

modifyLRef o (Sum . length)
M.readMutVar t'
--[Sum {getSum = 2},Sum {getSum = 5}]

modifyLRef o' ("oh"++)
M.readMutVar s'
["ohhi","ohthere"]

-}

type Reads d a = (d (Const a) :: Constraint)
type Writes d = (d Identity :: Constraint)
class (f :: * -> * -> *) ~ (->) => Function f
instance Function (->)

type Ocular c d s t a b = forall p f. (c p, d f) => L.Optic p f s t a b -- p a (f b) -> p s (f t)


-- | Abstraction over a tuple of mutable references. The first reference is read-only.
data RefST r c d s t a b = RefST (Ocular c d s t a b) (MutVar r s) (MutVar r t)

-- id :: Equality' s s
id_ref :: PrimMonad m => MutVar (PrimState m) s -> RefST (PrimState m) L.Profunctor Functor s s s s
id_ref rs = RefST id rs rs

-- non :: Eq a => a -> Iso' (Maybe a) a
non_ref :: (PrimMonad m, Eq s) => s -> MutVar (PrimState m) (Maybe s) -> RefST (PrimState m) L.Profunctor Functor (Maybe s) (Maybe s) s s 
non_ref s rs = RefST (L.non s) rs rs

-- only :: Eq a => a -> Prism' a ()
only_ref :: (PrimMonad m, Eq s) => s -> MutVar (PrimState m) s -> RefST (PrimState m) L.Choice Applicative s s () ()
only_ref s rs = RefST (L.only s) rs rs

-- united :: Lens' a ()
united_ref :: PrimMonad m => MutVar (PrimState m) s -> RefST (PrimState m) Function Functor s s () () 
united_ref rs = RefST L.united rs rs

-- both :: Bitraversable r => Traversal (r a a) (r b b) a b
both_ref :: (PrimMonad m, Bitraversable r) => MutVar (PrimState m) (r a a) -> MutVar (PrimState m) (r b b) -> RefST (PrimState m) Function Applicative (r a a) (r b b) a b
both_ref rs rt = RefST L.both rs rt

-- traverse :: Traversable f => Traversal (f s) (f t) s t
traverse_ref :: (PrimMonad m, Traversable f) => MutVar (PrimState m) (f s) -> MutVar (PrimState m) (f t) -> RefST (PrimState m) Function Applicative (f s) (f t) s t 
traverse_ref rs rt = RefST traverse rs rt




data RRef r c d a b = forall s t. RRef (Ocular c d s t a b) (MutVar r s) (MutVar r t)  -- { unRRef :: forall s t. RefST r c d s t a b }
--type LRef =  RefST r c d s t a b




newLocalRRef :: PrimMonad m => s -> t -> Ocular c d s t a b -> m (RRef (PrimState m) c d a b)
newLocalRRef s t o = (RRef o) <$> M.newMutVar s <*> M.newMutVar t 

newRRef :: PrimMonad m => MutVar (PrimState m) s -> MutVar (PrimState m) t -> Ocular c d s t a b -> RRef (PrimState m) c d a b
newRRef s t o = RRef o s t 

{-
newLRef :: forall a b c d m s t. (PrimMonad m) => MutVar (PrimState m) s -> MutVar (PrimState m) t -> Ocular Function Applicative s t a b -> RRef (PrimState m) c d a b
newLRef = newRRef @m @_ @_ @Function @Applicative
-}

newLRef
  :: MutVar RealWorld w1
     -> MutVar RealWorld w2
     -> Ocular Function Functor w1 w2 a b
     -> RRef RealWorld Function Functor a b
newLRef = newRRef @IO @_ @_ @Function @Functor

newPRef
  :: MutVar RealWorld w1
     -> MutVar RealWorld w2
     -> Ocular L.Choice Applicative w1 w2 a b
     -> RRef RealWorld L.Choice Applicative a b
newPRef = newRRef @IO @_ @_ @L.Choice @Applicative


data Foo = Foo { _a :: Int, _b :: String }
L.makeLenses ''Foo

--a :: Functor f => (Int -> f Int) -> Foo -> f Foo
--b :: Functor f => (String -> f String) -> Foo -> f Foo

a_rref :: PrimMonad m => Foo -> m (RRef (PrimState m) Function Functor Int Int)
a_rref s = newLocalRRef s s a

b_rref :: PrimMonad m => Foo -> m (RRef (PrimState m) Function Functor String String)
b_rref s = newLocalRRef s s b


modifyPRef
  :: PrimMonad m
  => c (L.Market a b)
  => Writes d
  => RRef (PrimState m) c d a b 
  -> (a -> b) 
  -> m ()
modifyPRef (RRef p rs rt) f = 
  do s <- M.readMutVar rs
     L.withPrism p $ \bt seta -> 
       either (M.writeMutVar rt) (M.writeMutVar rt . bt . f) (seta s)

readLRef
  :: (c (->), Function (->), d (Const a), PrimMonad f) =>
     RRef (PrimState f) c d a b -> f a
--readLRef :: (PrimMonad m, Reads Applicative a) => RRef (PrimState m) Function Applicative a b -> m a
readLRef (RRef l rs _) = get <$> M.readMutVar rs where get s' = withGetter l $ \acsc -> acsc id s' 


modifyLRef :: (PrimMonad m, Writes d, c (->)) => RRef (PrimState m) c d a b -> (a -> b) -> m ()
modifyLRef (RRef l rs rt) f = 
  do s <- M.readMutVar rs
     withSetter l $ \abst -> 
       M.writeMutVar rt $! abst f s



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




