An algorithm for inference
==========================

We are ostensibly doing untyped lambda calculus
so we need the common language extenstions

> {-# LANGUAGE GADTs, DataKinds, KindSignatures #-}

We need a type to represent variables

> type Var = Int

The "is this an abstraction" hole

> data Hole = Yes | No

Right hand side values

> data Rhs :: Hole -> * where
>    Var :: Var -> Rhs No
>    Abs :: Var -> Rhs No -> Rhs Yes 
>    Mu  :: Rhs Yes -> Rhs No
>    Fun :: Var -> Rhs Yes
>    App :: Rhs Yes -> Var -> Rhs No
>    Prd :: Rhs No -> Rhs No -> Rhs No

> instance Show (Rhs a) where
>    show (Var x)     = "v"++show x
>    show (Mu rhs)    = "mu "++(show rhs)
>    show (Abs x rhs) = "(\\v"++(show x)++" -> "++(show rhs)++")"
>    show (Fun x)     = "F"++(show x)
>    show (App rhs x) = (show rhs) ++ " v"++(show x)
>    show (Prd r l)   = "("++(show r)++", "++(show l)++")"

The equations we are interested in 

> data Equation = Eqn Var (Rhs No)

> instance Show Equation where
>    show (Eqn v rhs) = "v"++(show v)++" = "++(show rhs)
