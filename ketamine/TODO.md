
- make MonadEnv polymorphic enough to support Conduit https://www.tweag.io/posts/2017-10-05-streaming2.html

makeInputStream :: MonadUnliftIO m => m (Maybe a) -> m (InputStream a)
makeInputStream f = withRunInIO $ \io -> S.makeInputStream (io $ f)

makeOutputStream :: MonadUnliftIO m => (Maybe a -> m ()) -> m (OutputStream a)
makeOutputStream f = withRunInIO $ \io -> S.makeOutputStream (io . f)

peek :: MonadUnliftIO m => InputStream a -> m (Maybe a)

runResourceT :: MonadUnliftIO m => ResourceT m a -> m a
runResourceT m = withRunInIO $ \run -> Res.runResourceT $ Res.transResourceT run m

liftResourceT :: MonadIO m => ResourceT IO a -> ResourceT m a
liftResourceT (ResourceT f) = ResourceT $ liftIO . f

transResourceT :: (m a -> n b)
               -> ResourceT m a
               -> ResourceT n b
transResourceT f (ResourceT mx) = ResourceT (\r -> f (mx r))


- dopamine-gym
http://hackage.haskell.org/package/http-conduit
https://www.yesodweb.com/blog/2014/03/network-conduit-async
http://hackage.haskell.org/package/pipes-http-1.0.6/docs/Pipes-HTTP.html


sqlite3 chinook.db
sqlite> .read examples/chinook.sql


import Database.Beam.Sqlite
conn <- open "chinook.db"


withDatabase conn $ runSelectReturningList $ select $ all_ $ album chinookDb

(select (all_ (_ chinookDb)))
