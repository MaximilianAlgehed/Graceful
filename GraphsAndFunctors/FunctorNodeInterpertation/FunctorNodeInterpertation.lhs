The Functor Node interpertation
===============================

Suppose we have the following diagram

\usetikzlibrary{graphs}
\usetikzlibrary{arrows}
\begin{figure}[H]
\centering
\begin{tikzpicture}[->, thick, auto, nd/.style={draw, circle, node distance=2cm, minimum width=2em}]
    \node[nd] (a) {\Large{$a$}};
    \node[nd, right of=a] (b) {\Large{$b$}};
    \path (a) edge node [above] {\Large{$\phi$}} (b);
\end{tikzpicture}
\end{figure}

representing some causal relationship between $a$ and $b$,
i.e. $a$ has some effect $\phi$ on $b$.

If one wanted to model this causal relationship with functions
one may give $\phi$ the type $\phi :: X \rightarrow F\ X$ for some type $X$ and functor $F$\footnote{The reason for the functor is
that one may wish to model, say, a list of possible influences from $a$ on $b$}.
From this assumption we arrive at

$a :: X$

$b :: F\ X$

Which is fine, as we may decide on a value of type $X$ for the node $b$ by any F-algebra $\rho :: F\ X \rightarrow X$.

However, for reasons that become clear when studying diagrams with feedback loops,
this interpertation is inadequate\footnote{See the set of derivations in the Type Node compendium}.
An interpertation where $a$ and $b$ are associated with functors $A$ and $B$ is more
desierable\footnote{See the derivations for cycles in this compendium}.
In this diagram the functor $B$ is completely decided by $A$ and $F$ as $A\ \phi :: A\ X\rightarrow A\ (F\ X)$ which
gives $B = A\circ F$.
