import Data.List 
import Data.Ord (comparing)
import Data.Monoid
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Select

import Control.Monad.IO.Class
-- http://eptcs.web.cse.unsw.edu.au/paper.cgi?MSFP2014.3.pdf
e :: Cont Bool Bool
e = cont $ \p -> p $ p True

s :: Select Bool Bool
s = select $ \p -> p True

eps :: SelectT Bool IO Bool
eps = SelectT ($ True)

sat :: Int -> ([Bool] -> IO Bool) -> IO [Bool]
sat n = runSelectT $ sequence $ replicate n $ eps

g :: [Bool] -> IO Bool
g bs = return $ bs!!0 && not(bs!!1) && bs!!2


{-
> sel 2 and
[True,True]
> sel 2 or
[True,True]
> sel 3 g
[True,False,True]
-}


{-
 -However, the classical SAT problem is only to decide whether a satisfying assignment exists, rather
than to actually compute one. By construction, if a formula q has a satisfying assignment then (⊗iε)q is
a satisfying assignment. Therefore q is satisfiable iff q((⊗iε)q) is true, that is, if (⊗iε)q is true. If the
product is finite this is equal to (⊗i∃)q, and ∃ can be written directly in Haskell as
 -
 -
-}

satCont :: Int -> ([Bool] -> Bool) -> Bool
satCont n = runCont $ sequence $ replicate n $ cont $ \p -> p $ p True

-- > sat 2 and
-- True
-- > sat 2 or
-- True

-- https://arxiv.org/pdf/1503.06061.pdf
--callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCC' :: ((a -> SelectT r m s) -> SelectT s m a) -> SelectT s m a
callCC' f = SelectT $ \ c -> runSelectT (f (\ x -> SelectT $ \ _ -> c x)) c

trace :: (MonadIO m) => String -> m ()
trace = liftIO . putStrLn

{-
> runContT foo return
In foo
In handler
In continuation
1
-}

foo :: ContT r IO Int
foo = do 
  trace "In foo"
  n <- callCC $ \k -> do trace "In handler"
                         m <- k 0
                         trace "Still in handler"
                         return (m + 1)
  trace "In continuation"
  return (n + 1)

{-
> runSelectT bar return
In foo
In handler
In continuation
Still in handler
In continuation
3
-}
bar :: Num r =>  SelectT r IO r
bar = do 
  trace "In foo"
  n <- callCC' $ \k -> do trace "In handler"
                          m <- k 0
                          trace "Still in handler"
                          return (m + 1)
  trace "In continuation"
  return (n + 1)


sat' :: Int -> SelectT Bool IO [Bool]
sat' n = do 
    bs <- callCC' $ 
        \k -> sequence $ replicate n $ 
            do b <- SelectT ($ True)
               liftIO $ putStr $ "b = " ++ show b ++ ", "
               k []
               return b
    trace $ "Continuation called with " ++ show bs
    return bs


--runSelectT (sat' 3) g
{-
 - b = True, Continuation called with []
b = True, Continuation called with []
b = True, Continuation called with []
Continuation called with [True,True,True]
b = False, Continuation called with []
Continuation called with [True,True,False]
b = False, Continuation called with []
b = True, Continuation called with []
Continuation called with [True,False,True]
b = True, Continuation called with []
Continuation called with [True,False,True]
b = True, Continuation called with []
b = True, Continuation called with []
b = True, Continuation called with []
Continuation called with [True,True,True]
b = False, Continuation called with []
Continuation called with [True,True,False]
b = False, Continuation called with []
b = True, Continuation called with []
Continuation called with [True,False,True]
b = True, Continuation called with []
Continuation called with [True,False,True]
[True,False,True]

(Note that each time the current continuation is called with the empty list it
also calls the formula f, but the expressions which index the empty list, which
would result in a runtime exception, are not evaluated due to the lazy semantics of Haskell.) It is already an open question how to explain patterns such
as T T T, T T F, T F T, T F T, T T T, T T F, T F T, T F T resulting from the operational
behaviour of the product of selection functions. The positions of the empty list
in this trace only seem to deepen the mystery.

-}





-- https://stackoverflow.com/questions/42378073/how-to-use-the-select-monad-to-solve-n-queens
validBoard :: [Int] -> Bool
validBoard qs = all verify (tails qs)
  where verify [] = True
        verify (x : xs) = and $ zipWith (\i y -> x /= y && abs (x-y) /= i) [1..] xs

nqueens :: Int -> [Int]
nqueens boardSize = runSelect (traverse selectColumn columns) validBoard
  where columns = replicate boardSize [1..boardSize]
 
selectColumn :: [a] -> Select Bool a 
selectColumn candidates = select $ \s -> head $ filter s candidates ++ candidates

{-
 - The sequence-based solution has the defect that putting a queen in a column doesn't rule out the possibility of choosing the same column for subsequent queens; we end up unnecesarily exploring doomed branches.

 - If we want our column choices to be affected by previous decisions, we need to go beyond Applicative and use a Monad.

 - The monadic version still has the problem that it only checks completed boards, when the original list-based solution backtracked as soon as a partially completed board was found to be have a conflict. I don't know how to do that using Select.
 -


nqueens' :: Int -> [Int]
nqueens' boardSize = fst $ runSelect (go ([],[1..boardSize])) (validBoard . fst)
  where
  go (cps,[]) = return (cps,[])
  go (cps,fps) = (select $ \s ->
    let candidates = map (\(z,zs) -> (z:cps,zs)) (oneOf fps)
    in  head $ filter s candidates ++ candidates) >>= go

oneOf :: [a1] -> [(a3, b)]
oneOf = undefined
-}


data Action = Cautious | Risky deriving (Show)
type Outcome = Int
type P a = [a]

q :: [Action] -> P Outcome
q [Cautious, Cautious] = [0]
q [Cautious, Risky]    = [-1, 0, 1]
q [Risky, Cautious]    = [-1, 0, 1]
q [Risky, Risky]       = [-2, -1, 0, 1, 2]

type Choice = P (Action, P Outcome) -> (Action, P Outcome)
-- Choice  = m (a, m o) -> (a, m o)
--
newtype Context a m o = Context { unContext :: m (a, m o) }

argopt :: Choice -> SelectT Outcome [] Action
argopt f = SelectT $ \p -> [fst $ f [(Cautious, p Cautious), (Risky, p Risky)]]

induction :: (Traversable t, Monad m) => t (SelectT r m a) -> (t a -> m r) -> m (t a)
induction = runSelectT . sequence

induction' :: (Traversable t, Monad m) => (a -> SelectT r m b) -> t a -> (t b -> m r) -> m (t b)
induction' f = runSelectT . traverse f

--opt :: Monoid (Context a m o) => (Context a m o -> (a, m o)) -> SelectT o m a
opt :: (Monad m, Monoid t) => (t -> (a, b)) -> SelectT r m a
opt f = SelectT $ \p -> return $ fst $ f mempty

riskymax, riskymin, cautiousmax, cautiousmin :: Choice
riskymax = maximumBy $ comparing $ maximum . snd
riskymin = minimumBy $ comparing $ minimum . snd
cautiousmax = riskymax . filter (all (>= (-1)) . snd)
cautiousmin = riskymin . filter (all (<= 1) . snd)

-- Outcome = (Observation, Reward)
-- type Phi / Value o m a = ContT o m a = (a -> m o) -> m o -- state-value function for a policy
-- type Eps / Agent o m a = SelectT o m a = (a -> m o) -> m a  -- action policy
-- type Context a m o = a -> m o -- MDP state transition

-- https://www.cs.bham.ac.uk/~mhe/papers/selection-escardo-oliva.pdf
--
---- designed to be decoupled from ML frameworks, names stolen from SICP
-- type EnvT / AppT o m a = ContT o m a = (o -> m a) -> m a -- apply some policy, or evaluate a Context

-- type ValT o m a = SelectT o m a = (o -> m a) -> m o  -- eval function for a policy, or apply function for a Context
-- type Policy o m a = o -> m a -- policy from e.g. vw or tf. o is an observation here

-- mkAgent :: Value o m a -> Agent o m a
-- mkAgent = selectToContT
--
-- es1, es2, es3, es4 :: [SelectT Outcome [] Action]
es1 = [argopt riskymax, argopt riskymin]
es2 = [argopt riskymax, argopt cautiousmin]
es3 = [argopt cautiousmax, argopt riskymin]
es4 = [argopt cautiousmax, argopt cautiousmin]

--solve :: Monad m => [SelectT Outcome m Action] -> m [Action]
solve
  :: (Traversable t, Monad m) 
  => (t a -> m r) -> t (SelectT r m a) -> m (t a)
solve q es = runSelectT (sequence es) q


strategy :: [SelectT r [] a] -> ([a] -> [r]) -> [a] -> [a]
strategy es q xs = head $ runSelectT eps (q . (xs ++))
  where eps = sequence $ drop (length xs) es

