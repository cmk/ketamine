import Data.List
import Control.Monad.State
import Control.Monad.Trans.Cont

-- Literals

data Literal = Positive Int | Negative Int deriving (Show, Eq)

negateLiteral :: Literal -> Literal
negateLiteral (Positive n) = Negative n
negateLiteral (Negative n) = Positive n

-- Internal state of algorithm

data DPLL = Leaf {simplified :: Bool, clauses :: [[Literal]]}
          | Node {trueBranch :: DPLL, falseBranch :: DPLL}

-- Top level of algorithm

dpll :: Int -> [[Literal]] -> Bool
dpll n = evalState s . initialState 
  where s = runContT (satN n) q

satN :: Int -> ContT Bool (State DPLL) [Bool]
satN n = sequence $ replicate n e

e :: ContT Bool (State DPLL) Bool
e = ContT $ \p -> p True >>= p -- transformer equiv of p $ p True  

q :: [Bool] -> State DPLL Bool
q bs = do 
  s <- get
  let (s', b) = queryState s bs 0
  put s'
  return b

-- Interactions with internal state

initialState :: [[Literal]] -> DPLL
initialState cs = Leaf {simplified = False, clauses = cs}

queryState :: DPLL -> [Bool] -> Int -> (DPLL, Bool)
queryState (Leaf False cs) bs n = queryState (Leaf True $ simplify cs) bs n
queryState s@(Leaf True []) _ _ = (s, True)
queryState s@(Leaf True [[]]) _ _ = (s, False)
queryState (Leaf True cs) bs n = queryState (Node l r) bs n where
           l = Leaf False ([Negative n] : cs)
           r = Leaf False ([Positive n] : cs)
queryState (Node l r) (True : bs) n = (Node l' r, b) where
           (l', b) = queryState l bs (n + 1)
queryState (Node l r) (False : bs) n = (Node l r', b) where
           (r', b) = queryState r bs (n + 1)

-- Operations on clause sets

simplify :: [[Literal]] -> [[Literal]]
simplify cs = if null units
              then cs
              else simplify (foldl (flip propagateUnit) cs' units) where
         (units, cs') = partition isUnitClause cs

isUnitClause :: [Literal] -> Bool
isUnitClause [_] = True
isUnitClause _ = False

propagateUnit :: [Literal] -> [[Literal]] -> [[Literal]]
propagateUnit [l] = (map $ filter (/= negateLiteral l))
                  . (filter $ notElem l)

