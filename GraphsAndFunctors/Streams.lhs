Graphs and the identity functor
===============================

Say we wanted to define a counting node in a graph like

\usetikzlibrary{graphs}
\usetikzlibrary{arrows}
\begin{figure}[H]
\centering
\begin{tikzpicture}[->, thick, auto, nd/.style={draw, circle, node distance=2cm, minimum width=2em}]
    \node[nd] (a) {\Large{$a$}};
    \path (a) edge[loop above] node [above] {\Large{$\phi$}} (a);
\end{tikzpicture}
\end{figure}

Where $\phi :: a \rightarrow F_0\ a$ and $\phi$.
We can determine the type for $a$ to be $a = F_0\ a \implies a = \mu F_0$.

In order to give this structure some meaning we can define $F_0\ a = (X \times a)$
for some $X$. Note that $\mu F_0$ is now a stream of $Xs$.

To give a simple example assume that $X = int$ and $\phi (x, a) = (x+1, (x, a))$,
the meaning of the node $a$ is now clear, it is a counting node.

An algebra for this node would be $getCount :: (Int, Int) \leftarrow Int$ where $getCount = outl$.

Haskell Implementation
======================

We build on the previous discussion

> import GraphsAndFunctors

We define the identity functor carrying the type $x$
as a value

> data ID x a = ID (x, a)
> instance Functor (ID x) where
>     fmap f (ID p) = ID $ fmap f p

> phi :: Mu (ID Int) -> Mu (ID Int)
> phi (In (ID (x, a))) = In $ ID (x+1, (In (ID (x, a))))

> getCount :: ID Int Int -> Int
> getCount (ID (x, _)) = x

> initialValue :: Mu (ID Int)
> initialValue = In $ ID (0, initialValue)
