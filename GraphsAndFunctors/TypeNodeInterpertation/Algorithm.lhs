Formal grammar for functor calculus
===================================

A grammar for our coalgebra graphs

\subsection{Coalgebra grammar}
\begin{grammar}
<var> ::= $v_0$ | $v_1$ | ...

<functor> ::= $F_0$ | $F_1$ | ...

<no hole> ::= <var>
\alt $\mu$ <hole>
\alt <hole> <no hole>
\alt <no hole> $\times$ <no hole>
\alt <no hole>[<var>/<no hole>]

<hole> ::= $\lambda$<var> $\rightarrow$ <no hole>
\alt <functor>
\alt <hole>[<var>/<no hole>]

<equation> ::= <var> $=$ <no hole>
\end{grammar}

\subsection{Semantic rules of the coalgebra grammar}

\inference[LamRem]
{
e\ :\ <no\ hole>\\
v,\ a : <var>\\
(\lambda a \rightarrow e)\ v
}
{e[a/v]}

\quad

\inference[VarRep(0)]
{
x\ :\ <var>\\
x[x/y]\\
}
{y}

\quad

\inference[VarRep(1)]
{
x\ :\ <hole>\\
(\mu\ x)[a/b]
}
{\mu\ (x[a/b])}

\quad

\inference[VarRep(3)]
{
x,\ y\ :\ <hole>\\
(x\times y)[a/b]
}
{(x[a/b])\times (y[a/b])}

\quad

\inference[EqnSubs]
{
v,\ w\ :\ <var>\\
e,\ f\ :\ <no hole>\\
v = e\\
w = f\\
}
{v = e[w/f]}

\quad

\inference[FixIntro]
{
v\ :\ <var>\\
f\ :\ <hole>\\
e\ :\ \{<equation>\}\\
e \Rightarrow v = fixpoint(f)\\
}
{v = \mu f}

\subsection{Some theorems}

\begin{lemma} \textbf{Beta equivalence lemma}\\
If $e$ is a term in our grammar and $a$ is a variable, then\\
$a = e \implies a\ = (\lambda \bar{a}\rightarrow e[a/\bar{a}])\ a$
\end{lemma}

\begin{proof}
Follows directly from \textit{LamRem} and \textit{VarRep(*)}
\end{proof}

\begin{lemma} \textbf{Fixpoint substitution lemma}\\
if $e$ is a term in our grammar and $a$ is a variable, then\\
$a = e \implies a = \mu (\lambda \bar{a}\rightarrow e[a/\bar{a}])$
\end{lemma}

\begin{proof}

\end{proof}

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
>        freeVariables' xs (App rhs rhs') = union
>                                               (freeVariables' xs rhs)
>                                               (freeVariables' xs rhs')
>        freeVariables' xs (Prd rhs rhs') = union
>                                               (freeVariables' xs rhs)
>                                               (freeVariables' xs rhs')
>        freeVariables' xs _              = empty

We need a function to replace all occurances of a variable
with it's definition.
\texttt{replace} has the following invariant:
The variable name to be replaced is not to be
used as a variable name in an abstraction.

> replace :: Var -> Rhs No -> Rhs No -> Rhs No
> replace w (Var x) y
>    | w == x                = y 
>    | otherwise             = Var x
> replace w (Mu rhs) y       = Mu $ replace' w rhs y
> replace w (App rhs rhs') y = App (replace' w rhs y) (replace w rhs' y)
> replace w (Prd rhs rhs') y = Prd (replace w rhs y) (replace w rhs' y)

> replace' :: Var -> Rhs Yes -> Rhs No -> Rhs Yes
> replace' w (Abs x rhs) y = Abs x $ replace w rhs y
> replace' w r y  = r

> fixify :: Equation -> Equation
> fixify (Eqn v rhs)
>    | v `elem` freeVariables rhs =
>       Eqn v $ Mu $ Abs freshVar $ replace v rhs (Var freshVar)
>    | otherwise = Eqn v rhs
>        where
>            freshVar = firstNonElem ((L.sort . toList) (freeVariables rhs))
>            firstNonElem []  = 0
>            firstNonElem [x] = x+1
>            firstNonElem (x:y:xs)
>                | x+1 < y = x+1
>                | otherwise = firstNonElem (y:xs)

> replaceWithDefinitions :: [Equation] -> [Equation]
> replaceWithDefinitions eqns = L.map execReplace eqns
>     where
>         execReplace eqn = L.foldl rep eqn eqns
>         rep (Eqn v rhs) (Eqn v' rhs')
>             | v == v'   = Eqn v rhs
>             | otherwise = Eqn v (replace v' rhs rhs')
