{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}


{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}


{-# LANGUAGE ScopedTypeVariables, TypeOperators , GADTs, DataKinds #-}

{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE ImpredicativeTypes #-}

 {-# OPTIONS_GHC -w #-}
-- | Environment values with stateful capabilities.
module Numeric.Ketamine.Util.Ref where


--import Numeric.Ketamine.Types
import Control.Applicative (Const(..))
--import Control.Monad.Primitive (PrimMonad, PrimState, RealWorld)
import Control.Monad.Reader (MonadReader)
import Data.Functor.Identity
import Data.Tuple (swap)


import Data.Bitraversable
--import GHC.Exts (Constraint)
--import qualified Control.Lens as L
--import qualified Control.Lens.Internal.Prism as L

import Control.Monad.Ref (MonadRef)

import Data.Function ((&)) 
import Mezzolens hiding (All)
--import qualified Mezzolens as M
import Mezzolens.Profunctor
import Mezzolens.Unchecked

import Data.Kind (Constraint, Type)

import qualified Control.Monad.Ref as M



data Has (c :: Type -> Constraint) where Has :: c t => t -> Has c

withHas :: Has c -> (forall a. c a => a -> r) -> r
withHas (Has a) f = f a

type family All (ts :: [(Type -> Type -> Type) -> Constraint]) (a :: Type -> Type -> Type) :: Constraint where
  All '[] a = () 
  All (t ': ts) a = (t a, All ts a) 



class (All '[Function, Wandering] p) => TraverseLike p
instance (All '[Function, Wandering] p) => TraverseLike p

class (All '[Strong, Choice] p) => AffineTraverseLike p
instance (All '[Strong, Choice] p) => AffineTraverseLike p

class (All '[OutPhantom, Wandering] p) => FoldLike p
instance (All '[OutPhantom, Wandering] p) => FoldLike p

class (f :: Type -> Type -> Type) ~ (->) => Function f
instance f ~ (->)  => Function f



type Ocular c s t a b = forall p. (c p) => p a b -> p s t
type Ocular' c s a = Ocular c s s a a

-- c : constraint, r : the domain / namespace the ref lives in
--
-- TODO consider renaming to PRef. These will turn into computations.
data Ref r c b a = forall s t. Ref (Ocular c s t a b) (r t) (r s)
data PRef r c b a = forall s t. PRef (Ocular c s t a b) (r t) (r s)

--data Ref x y c b a = forall s t. Ref (Ocular c s t a b) (y t) (x s)

type Ref' r c a = Ref r c a a

withRef :: Ref r c b a -> (forall s t. Ocular c s t a b -> r t -> r s -> x) -> x
withRef (Ref o rt rs) f = f o rt rs

instance Profunctor (Ref r Profunctor) where
  --dimap :: (b -> t) -> (s -> a) -> p t s -> p b a
  dimap bt sa (Ref o rt rs) = Ref (o . iso sa bt) rt rs

--newRef :: (MonadRef x m, MonadRef y n) => Ocular c s t a b -> x s -> y t -> Ref x y c b a
newRef :: MonadRef r m => Ocular c s t a b -> r t -> r s -> Ref r c b a
newRef o t s = Ref o t s

newRef' :: MonadRef r m => Ocular' c s a -> r s -> Ref' r c a
newRef' o s = Ref o s s

--globalRef :: MonadRef r m => Ocular c s t a b -> t -> s -> Ref r c b a -- need unsafePerformIO for m
-- LocalRef c s a = LocalRef  { unLocalRef :: MonadRef r m => forall r. ReaderT (Ref r s) (ST r) a }
newLocalRef :: MonadRef r m => Ocular c s t a b -> t -> s -> m (Ref r c b a)
newLocalRef o t s = (Ref o) <$> M.newRef t <*> M.newRef s

--newLocalRef :: MonadRef r m => Ocular c s t a b -> t -> s -> Local (Ref r c b a) m a
--newLocalRef o t s = (Ref o) <$> M.newRef t <*> M.newRef s

--
newLocalRef' :: MonadRef r m => Ocular' c s a -> s -> m (Ref' r c a)
newLocalRef' o s = newLocalRef o s s 

readRef 
  :: MonadRef r m
  => c (Kleisli (Const a))
  => Ref r c b a 
  -> m a
readRef (Ref o _ s) = 
 let with s' = withGetter o $ \acsc -> acsc id s' 

  in with <$> M.readRef s 

modifyRef 
  :: MonadRef r m
  => c (Kleisli Identity) 
  => Ref r c b a 
  -> (a -> b) 
  -> m ()
modifyRef (Ref o rt rs) f = 
  do s <- M.readRef rs
     withSetter o $ \abst -> 
       M.writeRef rt $! abst f s

{-

modifyRef
  :: (c (Kleisli Identity), PrimMonad m) 
  => Ref c (PrimState m) b a -> (a -> b) -> m ()
modifyRef (Ref l rt rs) f = 
  do s <- M.readRef rs
     withSetter l $ \abst -> 
       M.writeRef rt $! abst f s
-}

withSetter
  :: Optical (SubStar Identity) s t a b
  -> (((a -> b) -> s -> t) -> r) 
  -> r
withSetter l f = f . cloneSetter $ l

cloneSetter :: Optical (SubStar Identity) s t a b -> (a -> b) -> s -> t
cloneSetter l afb = runIdentity . (modifyF @Identity l) (Identity . afb)

--withGetter :: LensLike (Const c) s t a b -> (((a -> c) -> s -> c) -> r) -> r
withGetter
  :: Optical (SubStar (Const c1)) s t a b
     -> (((a -> c1) -> s -> c1) -> c2) -> c2
withGetter l f = f . cloneGetter $ l

--cloneGetter :: LensLike (Const c) s t a b -> (a -> c) -> s -> c
cloneGetter 
  :: forall c s t a b. ()
  => Optical (SubStar (Const c)) s t a b
  -> (a -> c) -> s -> c
cloneGetter l afb = getConst . (modifyF @(Const c) l) (Const . afb)


--modifyL (Ref2 l s t) f = foo =<< readIORef2 s  where foo s' = withLens l $ \sa bst -> (writeIORef2 t $ bst (f . sa $ s') s')

--withLens :: Lens s t a b -> ((s -> a) -> (b -> s -> t) -> r) -> r
--withLens l f = f . _ $ l

-- contains :: Ord k => k -> Lens' (Set k) Bool
-- lens . ($ at)
-- r ~ (s -> b -> t) -> r
--cloneLens :: Optical p s t a b -> (s -> a)
--cloneLens = undefined


bar
  :: Strong p =>
     (Optical p ta tb a b -> t) -> (ta -> a) -> (b -> ta -> tb) -> t
bar l f g = l $ lens f g


baz :: Strong p => (Optical p b tb b tb -> t) -> t
baz l = l $ lens id const


--Found hole: _ :: Optical p0 ta0 tb0 a0 b0 -> t0
--  _ :: Optical p s t a b -> Optical p0 ta0 tb0 a0 b0 -> t0
--
--withLens :: Strong p => Optical p s t a b -> ((s -> a) -> (b -> s -> t) -> r) -> r
withLens
  :: Strong p =>
     (Optical p ta tb a b -> t1)
     -> (((ta -> a) -> (b -> ta -> tb) -> t1) -> t2) -> t2
withLens l f = f $ \sa bst -> (l $ (lens sa bst))


{-




newRef2 :: PrimMonad m => s -> t -> LensLike f s t a b -> m (Ref2 (PrimState m) f a b)
newRef2 s t o = (Ref2 o) <$> M.newRef s <*> M.newRef t 

newRef2' :: PrimMonad m => s -> LensLike' f s a -> m (Ref2 (PrimState m) f a a)
newRef2' s o = do
    s' <- M.newRef s 
    return $ Ref2 o s' s'


modifyRef2 :: PrimMonad m => Ref2 (PrimState m) Identity a b -> (a -> b) -> m ()
modifyRef2 (Ref2 l s t) f = with =<< M.readRef s  
    where with s' = withSetter l $ \abst -> (M.writeRef t $! abst f s')

--withLens :: Lens s t a b -> ((s -> a) -> (s -> b -> t) -> r) -> r

--TODO use M.atomicModifyRef internally
--use an alternate imple for PRef' r c a that writes back with the fst a?
--
atomicModifyRef2 :: PrimMonad m => Ref2 (PrimState m) a b -> (a -> (a, b)) -> m b
atomicModifyRef2 (Ref2 l s t) f = with =<< M.readRef s 
    where with s' = withLens l $ \sa sbt -> do
                        let (a, b) = f . sa $! s'
                        M.writeRef t $! sbt s' b (f . sa $! s')
                        return b


atomicModifyPRef :: PrimMonad m => PRef (PrimState m) a b -> (a -> (b, r)) -> m r
atomicModifyPRef (Ref2 l s t) f = with =<< M.readRef s 
    where with s' = withLens l $ \sa sbt -> do
                        let (a, b) = f . sa $! s'
                        M.writeRef t $! sbt s' b 
                        return b
                        let ssa f = \s -> let (b, r) =  f . sa $! s in (sbt s b, r)

atomicModifyPRef' :: PrimMonad m => PRef' (PrimState m) a -> (a -> (a, r)) -> m r

    where with s' = withLens l $ \sa sas -> do
                        let (a, r) =  f . sa $! s'
                             ssr f = \s -> in (sas s a, r)
                        M.atomicModifyRef' s' ssr

 
s = ("hi!",2) :: (String, Int)
t = (4,2)  :: (Int, Int)

rs <- M.newRef s
rt <- M.newRef t
(o :: Ref Strong RealWorld String Int) = newRef @IO _1 rt rs
(o' :: Ref Strong RealWorld String String) = newRef @IO _1 rs rs


modifyRef o' tail >> M.readRef rs >>= print >> M.readRef rt >>= print
-- ("i!",2)
--(4,2)


modifyRef o length >> M.readRef rs >>= print >> M.readRef rt >>= print
-- ("i!",2)
-- (2,2)


s = Just "hi!" :: Maybe String
t = Nothing  :: Maybe Int
rs <- M.newRef s
rt <- M.newRef t

(o :: Ref Choice RealWorld String Int) = newRef @IO _Just rt rs
(o' :: Ref Choice RealWorld String String) = newRef @IO _Just rs rs

modifyRef o' tail >> M.readRef rs >>= print >> M.readRef rt >>= print
Just "i!"
Nothing

modifyRef o length >> M.readRef rs >>= print >> M.readRef rt >>= print
Just "i!"
Just 2

-}




{-
import Data.Monoid

s = ["hi", "there"] :: [String]
t = fmap Sum [1..10] :: [Sum Int]

rs <- M.newRef s
rt <- M.newRef t

(o' :: Ref TraverseLike RealWorld String String) = newRef @IO @TraverseLike traverse rs rs

o = newRef s' t' traverse 
o' = newRef s' s' traverse 

readRef o
--"hithere"

modifyRef o (Sum . length)
M.readRef t'
--[Sum {getSum = 2},Sum {getSum = 5}]

modifyLRef o' ("oh"++) >> M.readRef rs >>= print
M.readRef s'
["ohhi","ohthere"]

-}


{-
type Reads d a = (d (Const a) :: Constraint)
type Writes d = (d Identity :: Constraint)
class (f :: * -> * -> *) ~ (->) => Function f
instance Function (->)

type Ocular c d s t a b = forall p f. (c p, d f) => L.Optic p f s t a b -- p a (f b) -> p s (f t)


data Ref r c d a b = forall s t. Ref (Ocular c d s t a b) (Ref r s) (Ref r t)  -- { unRef :: forall s t. RefST r c d s t a b }

data RefS r t a b = forall s. RefS (Ocular Function Functor s t a b) (Ref r s) (Ref r t)

--data RefT r c d s a b = forall t. RefT (Ocular c d s t a b) (Ref r s) (Ref r t)

-- | Abstraction over a tuple of mutable references. The first reference is read-only.
data RefST r c d s t a b = RefST (Ocular c d s t a b) (Ref r s) (Ref r t)

-- id :: Equality' s s
id_ref :: PrimMonad m => Ref (PrimState m) s -> RefST (PrimState m) L.Profunctor Functor s s s s
id_ref rs = RefST id rs rs

-- non :: Eq a => a -> Iso' (Maybe a) a
non_ref :: (PrimMonad m, Eq s) => s -> Ref (PrimState m) (Maybe s) -> RefST (PrimState m) L.Profunctor Functor (Maybe s) (Maybe s) s s 
non_ref s rs = RefST (L.non s) rs rs

-- only :: Eq a => a -> Prism' a ()
only_ref :: (PrimMonad m, Eq s) => s -> Ref (PrimState m) s -> RefST (PrimState m) L.Choice Applicative s s () ()
only_ref s rs = RefST (L.only s) rs rs

foo_ref :: (PrimMonad m) => Ref (PrimState m) s -> RefST (PrimState m) Strong Functor s s () ()
foo_ref rs = RefST L.united rs rs


-- united :: Lens' a ()
united_ref :: PrimMonad m => Ref (PrimState m) s -> RefST (PrimState m) Function Functor s s () () 
united_ref rs = RefST L.united rs rs

-- both :: Bitraversable r => Traversal (r a a) (r b b) a b
both_ref :: (PrimMonad m, Bitraversable r) => Ref (PrimState m) (r a a) -> Ref (PrimState m) (r b b) -> RefST (PrimState m) Function Applicative (r a a) (r b b) a b
both_ref rt rs = RefST L.both rt rs

-- traverse :: Traversable f => Traversal (f s) (f t) s t
traverse_ref :: (PrimMonad m, Traversable f) => Ref (PrimState m) (f s) -> Ref (PrimState m) (f t) -> RefST (PrimState m) Function Applicative (f s) (f t) s t 
traverse_ref rt rs = RefST traverse rt rs



newLocalRef :: PrimMonad m => s -> t -> Ocular c d s t a b -> m (Ref (PrimState m) c d a b)
newLocalRef s t o = (Ref o) <$> M.newRef s <*> M.newRef t 

newRef :: PrimMonad m => Ref (PrimState m) s -> Ref (PrimState m) t -> Ocular c d s t a b -> Ref (PrimState m) c d a b
newRef s t o = Ref o s t 

{-
newLRef :: forall a b c d m s t. (PrimMonad m) => Ref (PrimState m) s -> Ref (PrimState m) t -> Ocular Function Applicative s t a b -> Ref (PrimState m) c d a b
newLRef = newRef @m @_ @_ @Function @Applicative
-}

newLRef
  :: Ref RealWorld w1
     -> Ref RealWorld w2
     -> Ocular Function Functor w1 w2 a b
     -> Ref RealWorld Function Functor a b
newLRef = newRef @IO @_ @_ @Function @Functor

newPRef
  :: Ref RealWorld w1
     -> Ref RealWorld w2
     -> Ocular L.Choice Applicative w1 w2 a b
     -> Ref RealWorld L.Choice Applicative a b
newPRef = newRef @IO @_ @_ @L.Choice @Applicative


data Foo = Foo { _a :: Int, _b :: String }
L.makeLenses ''Foo

--a :: Functor f => (Int -> f Int) -> Foo -> f Foo
--b :: Functor f => (String -> f String) -> Foo -> f Foo

a_rref :: PrimMonad m => Foo -> m (Ref (PrimState m) Function Functor Int Int)
a_rref s = newLocalRef s s a

b_rref :: PrimMonad m => Foo -> m (Ref (PrimState m) Function Functor String String)
b_rref s = newLocalRef s s b


modifyPRef
  :: PrimMonad m
  => c (L.Market a b)
  => Writes d
  => Ref (PrimState m) c d a b 
  -> (a -> b) 
  -> m ()
modifyPRef (Ref p rt rs) f = 
  do s <- M.readRef rs
     L.withPrism p $ \bt seta -> 
       either (M.writeRef rt) (M.writeRef rt . bt . f) (seta s)

readLRef
  :: (c (->), Function (->), d (Const a), PrimMonad f) =>
     Ref (PrimState f) c d a b -> f a
--readLRef :: (PrimMonad m, Reads Applicative a) => Ref (PrimState m) Function Applicative a b -> m a
readLRef (Ref l rs _) = get <$> M.readRef rs where get s' = withGetter l $ \acsc -> acsc id s' 


modifyLRef :: (PrimMonad m, Writes d, c (->)) => Ref (PrimState m) c d a b -> (a -> b) -> m ()
modifyLRef (Ref l rt rs) f = 
  do s <- M.readRef rs
     withSetter l $ \abst -> 
       M.writeRef rt $! abst f s






withSetter :: LensLike Identity s t a b -> (((a -> b) -> s -> t) -> r) -> r
withSetter l f = f . cloneSetter $ l

cloneSetter :: LensLike Identity s t a b -> ((a -> b) -> s -> t)
cloneSetter l afb = runIdentity . l (Identity . afb)

withGetter :: LensLike (Const c) s t a b -> (((a -> c) -> s -> c) -> r) -> r
withGetter l f = f . cloneGetter $ l

cloneGetter :: LensLike (Const c) s t a b -> (a -> c) -> s -> c
cloneGetter l afb = getConst . l (Const . afb)

-}


{-
{-

-- | Abstraction over a tuple of mutable references. The first reference is read-only.
-- Use 'newRef2'' to mutate the first reference with a 'LensLike''.
data Ref2 x f a b = forall s t . Ref2 (LensLike f s t a b) (Ref x s) (Ref x t)

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


--data Ref2 x a b = forall s t . Ref2 (Lens s t a b) (Ref x s) (Ref x t)
--type LensRef2 x a b = forall f . Functor f => Ref2 x f a b

newRef2 :: PrimMonad m => s -> t -> LensLike f s t a b -> m (Ref2 (PrimState m) f a b)
newRef2 s t o = (Ref2 o) <$> M.newRef s <*> M.newRef t 

newRef2' :: PrimMonad m => s -> LensLike' f s a -> m (Ref2 (PrimState m) f a a)
newRef2' s o = do
    s' <- M.newRef s 
    return $ Ref2 o s' s'

readRef2 :: PrimMonad m => Ref2 (PrimState m) (Const a) a b -> m a
readRef2 (Ref2 l s _) = with <$> M.readRef s  
    where with s' = withGetter l $ \acsc -> acsc id s' 

modifyRef2 :: PrimMonad m => Ref2 (PrimState m) Identity a b -> (a -> b) -> m ()
modifyRef2 (Ref2 l s t) f = with =<< M.readRef s  
    where with s' = withSetter l $ \abst -> (M.writeRef t $! abst f s')
--modifyL (Ref2 l s t) f = foo =<< readIORef2 s  where foo s' = withLens l $ \sa sbt -> (writeIORef2 t $ sbt s' (f . sa $ s'))

--withLens :: Lens s t a b -> ((s -> a) -> (s -> b -> t) -> r) -> r

atomicModifyRef2 :: PrimMonad m => Ref2 (PrimState m) a b -> (a -> (a, b)) -> m b
atomicModifyRef2 (Ref2 l s t) f = with =<< M.readRef s 
    where with s' = withLens l $ \sa sbt -> do
                        let (a, b) = f . sa $! s'
                        M.writeRef t $! sbt s' b (f . sa $! s')
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

-}



{-
import Lens.Micro.GHC

im = IntMap.fromList [(1,"hi"), (2,"there")]
im' <- M.newRef im

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

s' <- M.newRef s
l = Ref traverse s'
> f a = (a ++ a, a)
> atomicModifyRef' l f
"hithere"
> readRef l
"hihitherethere"
> M.readRef s'
["hihi","therethere"]
> g a = (a,"bar")
> atomicModifyRef' l g
"barbar"
> M.readRef s'
["hihi","therethere"]
-}

{-
data Ref x a = forall s . Ref (Lens' s a) (Ref x s)

type IORef = Ref RealWorld
type STRef s = Ref s

newRef :: PrimMonad m => Ref (PrimState m) s -> Lens' s a -> Ref (PrimState m) a
newRef s l = Ref l s

-- newGlobalRef --use logger-simple trick
-- newLocalRef 

readRef :: PrimMonad m => Ref (PrimState m) a -> m a
readRef (Ref l r) = view l <$> M.readRef r

writeRef :: PrimMonad m => Ref (PrimState m) a -> a -> m ()
writeRef (Ref l r) a = M.modifyRef r (set l a)

modifyRef :: PrimMonad m => Ref (PrimState m) a -> (a -> a) -> m ()
modifyRef (Ref l r) f = M.modifyRef r (over l f)

atomicModifyRef
  :: PrimMonad m => Ref (PrimState m) a -> (a -> (a, b)) -> m b
atomicModifyRef (Ref l r) f = M.atomicModifyRef r (swap . traverseOf l (swap . f))

-- | Strict version of 'writeRef'.
writeRef' :: PrimMonad m => Ref (PrimState m) a -> a -> m ()
writeRef' (Ref l r) a = M.modifyRef' r (set l a)

-- | Strict version of 'modifyRef'.
modifyRef' :: PrimMonad m => Ref (PrimState m) a -> (a -> a) -> m ()
modifyRef' (Ref l r) f = M.modifyRef' r (over l f)

-- | Strict version of 'atomicModifyRef'.
atomicModifyRef'
  :: PrimMonad m => Ref (PrimState m) a -> (a -> (a, b)) -> m b
atomicModifyRef' (Ref l r) f = M.atomicModifyRef' r (swap . traverseOf l (swap . f))

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
-}
