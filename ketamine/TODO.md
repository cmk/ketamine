
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


(select (all_ (_ chinookDb)))

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

