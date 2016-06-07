Cycles in the Functor Node interperatation
==========================================

Suppose we have the following diagram

\usetikzlibrary{graphs}
\usetikzlibrary{arrows}
\begin{figure}[H]
\centering
\begin{tikzpicture}[->, thick, auto, nd/.style={draw, circle, node distance=2cm, minimum width=2em}]
    \node[nd] (a) {\Large{$a\ X$}};
    \node[nd, right of=a] (b) {\Large{$b\ X$}};
    \path (a) edge[bend left=45] node [above] {\Large{$a^\prime\ \phi_0$}} (b);
    \path (b) edge[bend left=45] node [below] {\Large{$b^\prime\ \phi_1$}} (a);
\end{tikzpicture}
\end{figure}

Where

$\phi_i :: X \rightarrow F_i\ X$

And

$f^\prime\ X = f\ X + X$

We arrive at

$b\ X = a^\prime\ (F_0\ X)$

$a\ X = b^\prime\ (F_1\ (a^\prime\ (F_0\ X)))$

Haskell Implementation
======================

> {-# LANGUAGE DeriveFunctor #-}

> data Prime f x = Prime (f x) | Id x deriving (Show)

> instance (Functor f) => Functor (Prime f) where
>     fmap f (Id x)    = Id (f x)
>     fmap f (Prime x) = Prime (fmap f x)

> data B x = B {unB :: (Prime A (F0 x))} deriving (Show, Functor)

> data A x = A {unA :: (Prime B (F1 (Prime A (F0 x))))} deriving (Show, Functor)

We need some example functors

> newtype F0 x = F0 [x] deriving (Show, Functor)
> newtype F1 x = F1 [x] deriving (Show, Functor)

A type for X

> type X = Double

And some example coalgebras

> phi0 :: X -> F0 X
> phi0 x = F0 [x/2-1, x, x*2+1]

> phi1 :: X -> F1 X
> phi1 x = F1 [(x+1)/2, x, (x-1)*2]

And finally, initial conditions

> initial :: B X
> initial = B (Id (F0 [0.0]))

This is a mess.
