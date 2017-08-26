Loops in the Functor Node interpretation
========================================

\usetikzlibrary{graphs}
\usetikzlibrary{arrows}
\begin{figure}[H]
\centering
\begin{tikzpicture}[->, thick, auto, nd/.style={draw, circle, node distance=2cm, minimum width=2em}]
    \node[nd] (a) {\Large{$a$}};
    \path (a) edge[loop above] node [above] {\Large{$\phi$}} (a);
\end{tikzpicture}
\end{figure}

Where (as per usual) $\phi :: X \rightarrow F\ X$.
In the functor node interpretation we get
(the rather gnarly) $A\ X = A\ (F\ X)$.
