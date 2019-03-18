


- make MonadEnv polymorphic enough to support Conduit https://www.tweag.io/posts/2017-10-05-streaming2.html

makeInputStream :: MonadUnliftIO m => m (Maybe a) -> m (InputStream a)
makeInputStream f = withRunInIO $ \io -> S.makeInputStream (io $ f)

makeOutputStream :: MonadUnliftIO m => (Maybe a -> m ()) -> m (OutputStream a)
makeOutputStream f = withRunInIO $ \io -> S.makeOutputStream (io . f)


data TestHarnessCapabilities = HarnCaps
    { _logger                  :: !Logger
    , _metricsObserver         :: !M.Observer
    , _campaignClient          :: !CampaignClient
    , _assignOfferClient       :: !OA.AssignOfferClient
    }

-- | Run a computation with access to 'TestHarnessCapabilities'
runCapabilities
    :: Ports
    -> CampaignClient
    -> ReaderT TestHarnessCapabilities IO a
    -> IO a
runCapabilities ports cc ma =
    withStdLogger Log.Info                $ \l ->
    MC.withStdObserver                    $ \m ->
    withAssignOfferClient assocCfg        $ \assoc ->
       runReaderT ma
           HarnCaps { _logger                  = l
                    , _metricsObserver         = m
                    , _campaignClient          = cc
                    , _assignOfferClient       = assoc
                    }



- dopamine-gym
http://hackage.haskell.org/package/http-conduit
https://www.yesodweb.com/blog/2014/03/network-conduit-async
http://hackage.haskell.org/package/pipes-http-1.0.6/docs/Pipes-HTTP.html


TODO: break 2 libs out (logfmt and lens-ref) and document

-- | Canonical Logging Interface
--
-- Minimalistic example:
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> :m Platform.Effect Platform.Log Data.Text
-- >>> :{
-- runStdLogging . flip evalStateT (0 :: Int) $ do
--     infoT "been here"
--     modify (+ (1 :: Int))
--     withContext (Namespace "banana") $ do
--         errT "ouch!"
--         modify (+ (1 :: Int))
--         withContext (Namespace "rpc" +++ field "moon" ("full" :: Text)) $ do
--             x <- get @Int
--             infoT "done that" `inContext` field "state" (show x)
--             debugT "Look, my password lol!" `inContext` field "password" ("s3cr3t" :: Text)
--             infoT "ok tschüss!"
--     infoT "epilogue"
-- :}
-- level="info" msg="been here" pid=65499 prog="<interactive>" loc="interactive:Ghci1:6:5"
-- level="error" msg="ouch!" ns="banana" pid=65499 prog="<interactive>" loc="interactive:Ghci1:9:9"
-- level="info" msg="done that" moon="full" ns="rpc" pid=65499 prog="<interactive>" state="2" loc="interactive:Ghci1:13:13"
-- level="info" msg="ok tschüss!" moon="full" ns="rpc" pid=65499 prog="<interactive>" loc="interactive:Ghci1:15:13"
-- level="info" msg="epilogue" pid=65499 prog="<interactive>" loc="interactive:Ghci1:16:5"
--

runStdLogging . flip evalStateT (0 :: Int) $ m

m :: (MonadReader r m,
      HasLogger r Logger, MonadIO m) =>
     StateT Int m ()
m = do
    infoT "been here"
    modify (+ (1 :: Int))
    withContext (Namespace "banana") $ do
        errT "ouch!"
        modify (+ (1 :: Int))
        withContext (Namespace "rpc" +++ field "moon" ("full" :: Text)) $ do
            x <- get
            infoT "done that" `inContext` field "state" (show x)
            debugT "Look, my password lol!" `inContext` field "password" ("s3cr3t" :: Text)
            infoT "ok tschüss!"
    infoT "epilogue"

git fetch origin --prune
git push force-with-lease origin
git merge --ff-only
tig
grom git rebase origin master
fzbranch
fzf
:load brings hidden 


:set -Wall
:set -fdefer-type-errors
:def rr \_ -> return (":set -fforce-recomp\n:reload\n:set -fno-force-recomp")

REF wip:

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
import qualified Control.Lens as L
import qualified Control.Lens.Internal.Bazaar as L
import qualified Control.Lens.Internal.Context as L

import Control.Lens.Type
import Control.Lens.Internal.Prism (Market, Market')
import Data.IORef
import Data.Tuple (swap)
import Data.Either (either)

--data Ref a b = forall s. Ref (IORef s) (Lens s s a b)
--
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

data Ref x p q f a b = forall s t . Ref (Optical p q f s t a b) (x s) (x t)

data Ref' x p q f a = forall s . Ref' (Optical' p q f s a) (x s)

type PRef x a b = forall p f. (L.Choice p, Applicative f) => Ref x p p f a b 
type LRef x a b = forall f. Functor f => Ref x (->) (->) f a b 

newRef :: s -> t -> Optical p q f s t a b -> IO (Ref IORef p q f a b)
newRef s t o = (Ref o) <$> newIORef s <*> newIORef t 

newRef' :: s -> Optical p q f s s a b -> IO (Ref IORef p q f a b)
newRef' s o = do
    s' <- newIORef s 
    return $ Ref o s' s'

--newPRef' :: s -> Prism s s a b -> IO (PRef IORef (Market' a) (Market a b) f a b)
--newPRef' = newRef'

--modifyP :: Ref IORef (Market a b) (Market a b) Identity a b -> (a -> b) -> IO ()
modifyP (Ref p s t) f = foo =<< readIORef s  where foo s' = L.withPrism p $ \bt seta -> either (writeIORef t) (writeIORef t . bt . f) (seta s')

--ASetter s s a b -> Ref IORef s -> (a -> b) -> IO ()
--modifyL :: Ref IORef p q f a b1 -> (b2 -> t) -> IO b3
--modifyL :: Ref IORef (->) (->) Identity a b -> (a -> b) -> IO ()
--modifyL (Ref l s t) f = foo =<< readIORef s  where foo s' = withLens l $ \sa sbt -> (writeIORef t $ sbt s' (f . sa $ s'))

--withLens :: Lens s t a b -> ((s -> a) -> (s -> b -> t) -> r) -> r
withLens = undefined

withSetter :: LensLike Identity s t a b -> (((a -> b) -> s -> t) -> r) -> r
withSetter l f = f . cloneSetter $ l

-- TODO use cloneSetter to unify modifyL & modifyP
cloneSetter :: LensLike Identity s t a b -> ((a -> b) -> s -> t)
cloneSetter l afb = runIdentity . l (Identity . afb)

cloneIndexedSetter
  :: L.Indexable i p =>
     (L.Indexed i a1 (Identity a2) -> a3 -> Identity c)
     -> p a1 a2 -> a3 -> c
cloneIndexedSetter l pafb = runIdentity . l (L.Indexed $ \i -> Identity . (L.indexed pafb i))

--withGetter :: Getter s a -> ((s -> a) -> r) -> r
withGetter :: LensLike (Const c) s t a b -> (((a -> c) -> s -> c) -> r) -> r
withGetter l f = f . cloneGetter $ l

cloneGetter :: LensLike (Const c) s t a b -> (a -> c) -> s -> c
cloneGetter l afb = getConst . l (Const . afb)

withFoo
  :: (L.Bizarre p1 w1, Applicative f, L.Sellable p2 w2) =>
     (p2 a1 (w2 a1 b1 b1) -> a2 -> w1 a3 b2 t)
     -> ((p1 a3 (f b2) -> a2 -> f t) -> c) -> c
withFoo l f = f . foo $ l

foo l f = L.bazaar f . l L.sell

view :: Ref IORef (->) (->) (Const a) a b -> IO a
view (Ref l s t) = foo <$> readIORef s  
  where foo s' = withGetter l $ \acsc -> acsc id s' 

over :: Ref IORef (->) (->) Identity a b -> (a -> b) -> IO ()
over (Ref l s t) f = foo =<< readIORef s  
  where foo s' = withSetter l $ \abst -> (writeIORef t $ abst f s')


--modifyPRef :: PRef IORef a b -> (a -> b) -> IO ()
over' :: Ref IORef (Market a b) (Market a b) Identity a b -> (a -> b) -> IO ()
over' (Ref p s t) f = foo =<< readIORef s
  where foo s' = L.withPrism p $ \bt seta -> either (writeIORef t) (writeIORef t . bt . f) (seta s')

preview :: Ref IORef (Market a b) (Market a b) Identity a b -> IO (Maybe a)
preview (Ref p s t) = foo <$> readIORef s
  where foo s' = L.withPrism p $ \_ seta -> either (\_ -> Nothing) Just (seta s')

--newLensRef :: PrimMonad m => s -> Lens s s a a -> m (RRef1 (PrimState m) a)
{-
http://hackage.haskell.org/package/primitive-0.6.4.0/docs/Data-Primitive-MutVar.html
also MVar, TVar, Ptr
class Mutable x where 
  newRef
  readRef
  writeRef
  modifyRef s f = readRef s >>= writeRef s . f
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
s = Just "hi" :: Maybe String
s = 5
s' = newIORef s
o' = Ref _Show s' s'
o = newRef' s _Nothing
modifyP o head
s = Right "hi" :: Either Int String
t = Left 3 :: Either Int Int
s' <- newIORef s
t' <- newIORef t
o = ORef _Right s' t'
p = PRef _Right s' t'
> modifyPRef2 p length
> readIORef s'
Right "hi"
> readIORef t'
Right 2
> readPRef p
Just "hi"
> modifyPRef2 p ((+2) . length)
> readPRef p
Just "hi"
> readIORef t'
Right 4
p <- newPRef s t _Right
modifyPRef p length
readPRef p
s = Right 4 :: Either String Int
t = Right 3 :: Either Int Int
p <- newPRef' 4 $ only 4
(p' :: PRef IORef String Int) <- newPRef s t _Left
p :: PRef IORef String Int
modifyPRef p' length
-}

{-
data LRef' x a = forall s . LRef' (Lens' s a) (x s)
data PRef x a b = forall s t. PRef (Prism s t a b) (x s) (x t)
data PRef' x a = forall s. PRef' (Prism' s a) (x s)
newPRef :: s -> t -> Prism s t a b -> IO (PRef IORef a b)
newPRef s t p = (PRef p) <$> newIORef s <*> newIORef t 
--
--
newPRef' :: s -> Prism' s a -> IO (PRef' IORef a)
newPRef' s p = (PRef' p) <$> newIORef s 
modifyPRef :: PRef IORef a b -> (a -> b) -> IO ()
modifyPRef (PRef p s t) f = foo =<< readIORef s
  where foo s' = withPrism p $ \bt seta -> either (writeIORef t) (writeIORef t . bt . f) (seta s')
modifyPRef' :: PRef' IORef a -> (a -> a) -> IO ()
modifyPRef' (PRef' p s) f = foo =<< readIORef s
  where foo s' = withPrism p $ \as sesa -> either (writeIORef s) (writeIORef s . as . f) (sesa s')
readPRef :: PRef IORef a b -> IO (Maybe a)
readPRef (PRef p s t) = foo <$> readIORef s
  where foo s' = withPrism p $ \_ seta -> either (\_ -> Nothing) Just (seta s')
newLRef' :: s -> Lens s s a a -> IO (LRef' IORef a)
newLRef' s l = newIORef s >>= \r -> return (LRef' l r) 
modifyLRef' :: LRef' IORef a -> (a -> a) -> IO ()
modifyLRef' (LRef' l r) f = modifyIORef r (over l f)
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


{-
Data.IORef.modifyIORef' :: IORef a -> (a -> a) -> IO ()
GHC.IORef.newIORef :: a -> IO (IORef a)
GHC.IORef.readIORef :: IORef a -> IO a
GHC.IORef.writeIORef :: IORef a -> a -> IO ()
type ScalarRef s = Ref Void s
--
-- | Read from a Ref
--
readRef :: MonadIO m => Ref k s -> k -> m s
readRef (Ref r _) k = liftIO $ r k
-- | Write to a Ref
--
writeRef :: MonadIO m => Ref k s -> k -> s -> m ()
writeRef (Ref _ w) k s = liftIO $ w k s
-- | Modify a Ref
-- This function is subject to change due to the lack of atomic operations
--
modifyRef :: MonadIO m => Ref k s -> k -> (s -> s) -> m ()
modifyRef (Ref r w) k f = liftIO $ fmap f (r k) >>= w k
toScalarRef :: IORef s -> Ref Void s
toScalarRef rf = do
    Ref (\_ -> readIORef rf)
            (\_ val -> modifyIORef' rf (\_ -> val))
-- | create a new boxed Ref
--
newScalarRef :: MonadIO m => s -> m (Ref Void s)
newScalarRef s = toScalarRef <$> newIORef s
-}

{-
--
-- | Read from a Ref
--
readRef :: MonadIO m => Ref a -> m a
readRef (Ref x _) = liftIO x
-- | Write to a Ref
--
writeRef :: MonadIO m => Ref a -> a -> m ()
writeRef (Ref _ x) = liftIO . x
-- | Modify a Ref
-- This function is subject to change due to the lack of atomic operations
--
modifyRef :: MonadIO m => Ref a -> (a -> a) -> m ()
modifyRef (Ref r w) f = liftIO $ fmap f r >>= w
ioRefToRef :: IORef a -> Ref a
ioRefToRef rf = do
    Ref (readIORef rf)
            (\val -> modifyIORef' rf (\_ -> val))
-- | create a new boxed Ref
--
newRef :: MonadIO m => a -> m (Ref a)
newRef a = do
    ioRefToRef <$> newIORef a
-}


