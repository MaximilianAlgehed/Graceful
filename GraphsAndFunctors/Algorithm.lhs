An algorithm for inference
==========================

We are ostensibly doing untyped lambda calculus
so we need the common language extenstions

> {-# LANGUAGE GADTs, DataKinds, KindSignatures, RankNTypes #-}
> import Data.Set
> import qualified Data.List as L

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
>    App :: Rhs Yes -> Rhs No -> Rhs No
>    Prd :: Rhs No -> Rhs No -> Rhs No

> instance Show (Rhs a) where
>    show (Var x)     = "v"++show x
>    show (Mu rhs)    = "mu "++(show rhs)
>    show (Abs x rhs) = "(\\v"++(show x)++" -> "++(show rhs)++")"
>    show (Fun x)     = "F"++(show x)
>    show (App rhs x) = (show rhs) ++"("++(show x)++")"
>    show (Prd r l)   = "("++(show r)++", "++(show l)++")"

The equations we are interested in 

> data Equation = Eqn Var (Rhs No)

> instance Show Equation where
>    show (Eqn v rhs) = "v"++(show v)++" = "++(show rhs)

In order for our algorithm to work, we need to find all the free variables
in a right hand side

> freeVariables :: Rhs a -> Set Var
> freeVariables rhs = freeVariables' [] rhs
>     where
>        freeVariables' :: [Var] -> Rhs a -> Set Var
>        freeVariables' xs (Var x)
>            | x `elem` xs                = empty
>            | otherwise                  = singleton x
>        freeVariables' xs (Abs x rhs)    = freeVariables' (x:xs) rhs
>        freeVariables' xs (Mu rhs)       = freeVariables' xs rhs
>        freeVariables' xs (App rhs rhs') = union (freeVariables' xs rhs) (freeVariables' xs rhs')
>        freeVariables' xs (Prd rhs rhs') = union (freeVariables' xs rhs) (freeVariables' xs rhs')
>        freeVariables' xs _              = empty

> replace :: Var -> Rhs No -> Var -> Rhs No
> replace w (Var x) y
>    | w == x                = Var y
>    | otherwise             = Var x
> replace w (Mu rhs) y       = Mu $ replace' w rhs y

> replace w (App rhs rhs') y = App (replace' w rhs y) (replace w rhs' y)
> replace w (Prd rhs rhs') y = Prd (replace w rhs y) (replace w rhs' y)

> replace' :: Var -> Rhs Yes -> Var -> Rhs Yes
> replace' w (Abs x rhs) y
>    | w == x                = Abs y $ replace w rhs y
>    | otherwise             = Abs x $ replace w rhs y
> replace' w r y  = r

> fixify :: Equation -> Equation
> fixify (Eqn v rhs)
>    | v `elem` freeVariables rhs = Eqn v $ Mu $ Abs freshVar $ replace v rhs freshVar
>    | otherwise = Eqn v rhs
>        where
>            freshVar = firstNonElem ((L.sort . toList) (freeVariables rhs))
>            firstNonElem []  = 0
>            firstNonElem [x] = x+1
>            firstNonElem (x:y:xs)
>                | x+1 < y = x+1
>                | otherwise = firstNonElem (y:xs)
