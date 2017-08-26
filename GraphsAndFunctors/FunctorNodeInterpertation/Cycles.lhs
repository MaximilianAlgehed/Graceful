Cycles in the Functor Node Interpretation
==========================================

Suppose we have the following diagram

\usetikzlibrary{graphs}
\usetikzlibrary{arrows}
\begin{figure}[H]
\centering
\begin{tikzpicture}[->, thick, auto, nd/.style={draw, circle, node distance=5cm, minimum width=2em}]
    \node[nd] (a) {\Large{$a\ X$}};
    \node[nd, right of=a] (b) {\Large{$b\ X$}};
    \path (a) edge[bend left=45] node [above] {\Large{$inl\circ (a^\prime\ \phi_0)$}} (b);
    \path (b) edge[bend left=45] node [below] {\Large{$inl\circ (b^\prime\ \phi_1)$}} (a);
\end{tikzpicture}
\end{figure}

Where

$\phi_i :: X \rightarrow F_i\ X$

And

$f^\prime\ X = f\ X + X$

We arrive at

$b\ X = a^\prime\ (F_0\ X)$

$a\ X = b^\prime\ (F_1\ x)$

Having constructed values using our coalgebras
$\phi_0$ and $\phi_1$, we would like to get back
to $X$ using some algebras $\rho_0$ and $\rho_1$.

The way we do this is by magic...

Haskell Implementation
======================

> {-# LANGUAGE DeriveFunctor #-}

> data Prime f x = Prime (f x) | Id x deriving (Show)

> instance (Functor f) => Functor (Prime f) where
>     fmap f (Id x)    = Id (f x)
>     fmap f (Prime x) = Prime (fmap f x)

> data B x = B (Prime A (F0 x)) deriving (Show, Functor)

> data A x = A (Prime B (F1 x)) deriving (Show, Functor)

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

The simplest value of type A x

> simpleA :: F1 X -> A X
> simpleA = A . Id

> simpleB :: F0 X -> B X
> simpleB = B . Id

> applyB :: (X -> F1 X) -> B X -> A X
> applyB f x = A $ Prime $ fmap f x

> applyA :: (X -> F0 X) -> A X -> B X
> applyA f x = B $ Prime $ fmap f x

We also need algebras

> rho0 :: F0 X -> X
> rho0 (F0 xs) = minimum xs

> rho1 :: F1 X -> X
> rho1 (F1 xs) = maximum xs

And we need a way to reduce the node values to values of interest.
I *think* these are catamorphisms but I am really not sure.

> reduceB :: B X -> (F0 X -> X, F1 X -> X) -> X
> reduceB (B (Id x)) (f, _)      = f x
> reduceB (B (Prime a)) p@(f, _) = reduceA (fmap f a) p

> reduceA :: A X -> (F0 X -> X, F1 X -> X) -> X
> reduceA (A (Id x)) (_, f)      = f x
> reduceA (A (Prime b)) p@(_, f) = reduceB (fmap f b) p
