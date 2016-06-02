{-# LANGUAGE StandaloneDeriving #-}
module SATSolver where
import Data.List
import Rewriter
import WFF
import Debug.Trace
import Data.List
import Control.Monad
import qualified Data.Map as M

data Var = V String | N String deriving (Show, Eq, Ord)

opposite :: Var -> Var -> Bool
opposite (V s) (N s') = s == s'
opposite (N s) (V s') = s == s'
opposite _ _ = False

type Clause = [Var]

type CNF = [Clause]

vars :: CNF -> [Var]
vars = concat

type Solution = [Var]

solve :: WFF -> [Solution]
solve = sat . nub . (map sort) . toSat

sat :: CNF -> [Solution]
sat = nub . (map sort) . sat'
    where
        sat' []  = [[]]
        sat' cnf = do
            v <- vars cnf
            let newForm = eliminate v cnf
            guard $ (not . unSat) newForm
            fmap (v:) $ sat newForm

unSat :: CNF -> Bool
unSat [] = False
unSat xs = any null xs

eliminate :: Var -> CNF -> CNF
eliminate v cnf = fiddle v cnf 
    where
        fiddle v [] = []
        fiddle v (x:xs) = if v `elem` x then fiddle v xs else (filter (not . (opposite v)) x):(fiddle v xs)

toSat :: WFF -> CNF
toSat wff = map nub $ (mkSat . (perform_rewrite [implFree, unNot, deMorgan, equivFree, distribute])) wff
    where
        mkSat (Var s) = [[V s]]
        mkSat (Or f g) = [concat $ (mkSat f) ++ (mkSat g)]
        mkSat (And f g) = (mkSat f) ++ (mkSat g)
        mkSat (Not (Var s)) = [[N s]]

implFree (Impl f g) = Just $ (Not f) .|| g
implFree _ = Nothing

deMorgan (Not (And f g)) = Just $ (Not f) .|| (Not g)
deMorgan (Not (Or f g))  = Just $ (Not f) .&& (Not g)
deMorgan _               = Nothing

unNot (Not (Not x)) = Just x
unNot _ = Nothing

equivFree (Eqv a b) = Just $ (a ==> b) .&& (b ==> a)
equivFree _ = Nothing

distribute (Or a (And b c)) = Just $ (Or a b) .&& (Or a c)
distribute (Or (And a b) c) = Just $ (Or a c) .&& (Or b c)
distribute _ = Nothing
