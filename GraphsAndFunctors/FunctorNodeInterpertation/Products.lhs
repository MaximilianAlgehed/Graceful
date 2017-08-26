The Functor Node interpretation
===============================

Suppose we have the following diagram

\usetikzlibrary{graphs}
\usetikzlibrary{arrows}
\begin{figure}[H]
\centering
\begin{tikzpicture}[->, thick, auto, nd/.style={draw, circle, node distance=2cm, minimum width=2em}]
    \node[nd] (a) {\Large{$a$}};
    \node[nd, right of=a] (c) {\Large{$c$}};
    \node[nd, right of=c] (b) {\Large{$b$}};
    \path (a) edge node [above] {\Large{$\phi_0$}} (c);
    \path (b) edge node [above] {\Large{$\phi_1$}} (c);
\end{tikzpicture}
\end{figure}

What is notable here is that $c$ depends on two nodes $a$ and $b$.

It is assumed we wish the $\phi$s to represent co-algebras of
some functors $F_0$ and $F_1$. In accordance to our previous efforts
of modelling the impact of the nodes $a$ and $b$ on $c$ by lifted
co-algebras we obtain the types

$A\ X = I\ X$

$B\ X = I\ X$

$C\ X = A\ (F_0\ X) \otimes B\ (F_1\ X)$

where $I$ denotes the identity functor and $\otimes$ is some bi-functor, which gives us

$c\ X = F_0\ X \otimes F_1\ X$

as there are no further constraints (i.e. constraints induced from a loop from $c$ to $c$)
we are free to conclude that $\otimes = \times$.
