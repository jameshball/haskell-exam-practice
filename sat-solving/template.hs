module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f1)
      = "(" ++ showF f ++ " & " ++ showF f1 ++ ")"
    showF (Or f f1)
      = "(" ++ showF f ++ " | " ++ showF f1 ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp item pairs
  = fromJust $ lookup item pairs

-- 3 marks
vars :: Formula -> [Id]
vars f
  = sort $ nub $ case f of
                   Var id     -> [id]
                   Not f1     -> vars f1
                   And f1 f2 -> vars f1 ++ vars f2
                   Or f1 f2  -> vars f1 ++ vars f2

-- 1 mark
idMap :: Formula -> IdMap
idMap f
  = zip (vars f) [1..]

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF f
  = case f of
      Not (Or f1 f2)  -> And (toNNF (Not f1)) (toNNF (Not f2))
      Not (And f1 f2) -> Or (toNNF (Not f1)) (toNNF (Not f2))
      Not (Not f1)     -> toNNF f1
      And f1 f2       -> And (toNNF f1) (toNNF f2)
      Or f1 f2        -> Or (toNNF f1) (toNNF f2)
      Not f1           -> Not (toNNF f1)
      Var id           -> Var id

-- 3 marks
toCNF :: Formula -> CNF
toCNF f
  = toCNF' $ toNNF f
  where
    toCNF' f
      = case f of
          And f1 f2 -> And (toCNF' f1) (toCNF' f2)
          Or f1 f2  -> distribute f1 f2
          _         -> f

-- 4 marks
flatten :: CNF -> CNFRep
flatten f
  = flatten' f
  where
    ids = idMap f
    flatten' f'
      = case f' of
          And f1 f2    -> flatten' f1 ++ flatten' f2
          Or f1 f2     -> [head (flatten' f1) ++ head (flatten' f2)]
          Not (Var id) -> [[negate $ lookUp id ids]]
          Var id       -> [[lookUp id ids]]


--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits f
  = propUnits' f (unitClauses f) []
  where
    propUnits' f' [] ucs
      = (f', ucs)
    propUnits' f' (u : cs) ucs
      = propUnits' newf (unitClauses newf) (u : ucs)
      where
        newf = map (filter (/= -u)) (filter (notElem u) f')

unitClauses :: CNFRep -> [Int]
unitClauses f
  = [u | [u] <- f]

-- 4 marks
dp :: CNFRep -> [[Int]]
dp []
  = [[]]
dp f
  | null ucs  = dp ([remvar] : f) ++ dp ([-remvar] : f)
  | elem [] f = []
  | otherwise = map (ucs' ++) (dp newf)
  where
    (newf, ucs') = propUnits f
    ucs = unitClauses f
    ((remvar : _) : _) = f

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat
  = undefined


