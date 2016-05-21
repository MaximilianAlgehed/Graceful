Interperting the structure of circular graphs
=============================================

Say we are interested in the relationships between the
types $a$ and $b$ in the below graph.

\usetikzlibrary{graphs}
\begin{figure}[H]
\centering
\begin{tikzpicture}[->, nd/.style={draw, circle, node distance=2cm, minimum width=2em}]
    \node[nd] (a) {\Large{$a$}};
    \node[nd, right of=a] (b) {\Large{$b$}};
    \path (a) edge[bend left=45] node [above] {\Large{$\phi_0$}} (b);
    \path (b) edge[bend left=45] node [below] {\Large{$\phi_1$}} (a);
\end{tikzpicture}
\end{figure}

Where $\phi_0$ and $\phi_1$ have the types
$\phi_0 :: a \rightarrow F_0\ a$ and $\phi_1 :: a \rightarrow F_1\ a$,
for some functors $F_0$ and $F_1$.

From this we can compute the types for $a$ and $b$

$a = F_1\ b$

$b = F_0\ a$

So we can conclude

$a = \mu (F_1 \circ F_0)$

$b = \mu (F_0 \circ F_1)$

To interperet the value at a node, let's assume we
have algebras $\bar{a}$ and $\bar{b}$

$\bar{a} :: F_0\ X \rightarrow X$

$\bar{b} :: F_1\ X \rightarrow X$

Then

$\bar{a}\circ (F_0\ \bar{b}) :: (F_0 \circ F_1)\ X \rightarrow X$

$\bar{b}\circ (F_1\ \bar{a}) :: (F_1 \circ F_0)\ X \rightarrow X$

From this we can construct the catamorphisms

$\llparenthesis \bar{a}\circ (F_0\ \bar{b}) \rrparenthesis :: \mu (F_0\circ F_1) \rightarrow X$

$\llparenthesis \bar{b}\circ (F_1\ \bar{a}) \rrparenthesis :: \mu (F_1\circ F_0) \rightarrow X$

Haskell Implementation
======================

We need a type for composition at the functor level

> data Compose f g a = C (f (g a))

Of course it's a functor

> instance (Functor f, Functor g) => Functor (Compose f g) where
>     fmap f (C a) = C $ (fmap . fmap) f a

We can construct an algebra from composition of two functors by
composition of two algebras

> comp_alg :: (Functor f) => (g a -> a) -> (f a -> a) -> Compose f g a -> a
> comp_alg inner outer (C x) = outer . (fmap inner) $ x

And we need to define fixpoints

> data Mu f = In (f (Mu f))

Assume we have some functor F0

> data F0 a
> instance Functor F0

and some functor F1

> data F1 b
> instance Functor F1

as well as a pair of F-algebras for these functors

> a :: F0 a -> a
> a = undefined
> b :: F1 a -> a
> b = undefined

Using the catamorphism from the Fixpoint of a functor

> cata :: (Functor f) => (f a -> a) -> Mu f -> a
> cata f (In a) = f (fmap (cata f) a)

We can extract the value from the structure of the functors we
constructed earlier

> cata_a :: Mu (Compose F0 F1) -> a
> cata_a = cata $ comp_alg b a
> cata_b :: Mu (Compose F1 F0) -> a 
> cata_b = cata $ comp_alg a b
