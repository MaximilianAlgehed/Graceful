{-# LANGUAGE DeriveDataTypeable #-}
module WFF where
import qualified Data.Map as M
import Test.QuickCheck hiding ((.||.), (==>), (.&&.))
import qualified Data.Set as S
import Data.Data
import Data.Generics.Uniplate.Data
import Data.List hiding (lookup, union)
import Prelude hiding (lookup)
import Control.Applicative hiding (empty)

data WFF = Var String
         | And WFF WFF
         | Or WFF WFF
         | Not WFF
         | Impl WFF WFF
         | Eqv WFF WFF deriving (Data)

instance Show WFF where
    show (Var s) = s
    show (And x y) = "("++(show x) ++ " && "++(show y)++")"
    show (Or x y)  ="("++(show x) ++ " || "++(show y)++")"
    show (Not x) = "~"++(show x)
    show (Impl x y) = "("++(show x) ++ "=>" ++ (show y)++")"
    show (Eqv x y)  = "("++(show x) ++ "=" ++ (show y) ++ ")"

instance Arbitrary WFF where
    arbitrary = sized myArbitrary
        where
            myArbitrary 0 = do
                                s <- oneof (map (return . show) [1..30])
                                return (Var s)
            myArbitrary n = oneof [ binary n And,
                                    binary n Or,
                                    binary n Impl,
                                    binary n Eqv,
                                    fmap Not (myArbitrary (n-1)),
                                    var
                                    ]
                                    where
                                        var = do
                                                s <- oneof (map (return . show) [1..30])
                                                return (Var s)
                                        binary n f = do
                                                    s <- myArbitrary (div n 2)
                                                    t <- myArbitrary (div n 2)
                                                    return (f s t)

t `xor` t' = (t .|| t') .&& (n (t .&& t'))
t .&& t' = And t t'
t .|| t' = Or t t'
t ==> t' = Impl t t'
t <=> t' = Eqv t t'
n t = Not t
v s = Var s

variables wff = [v | Var v <- universe wff]

fromJust (Just x) = x
fromJust _        = undefined

eval :: M.Map String Bool -> WFF -> Bool
eval m (Var s)   = fromJust $ M.lookup s m
eval m (And x y) = (&&) (eval m x) (eval m y)
eval m (Or x y)  = (||) (eval m x) (eval m y)
eval m (Not y)   = not  (eval m y)
eval m (Impl x y) = (not (eval m x)) || (eval m y)
eval m (Eqv x y) = (==) (eval m x) (eval m y)
