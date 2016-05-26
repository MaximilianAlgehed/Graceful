More complicated streams
========================

Consider the graph below

\usetikzlibrary{graphs}
\usetikzlibrary{arrows}
\begin{figure}[H]
\centering
\begin{tikzpicture}[->, thick, auto, nd/.style={draw, circle, node distance=2cm, minimum width=2em}]
    \node[nd] (a) {\Large{$a$}};
    \node[nd, right of=a] (b) {\Large{$b$}};
    \path (a) edge[bend left=45] node [above] {\Large{$\phi_1$}} (b);
    \path (b) edge[bend left=45] node [below] {\Large{$\phi_2$}} (a);
    \path (a) edge[loop above] node [above] {\Large{$\phi_0$}} (a);
\end{tikzpicture}
\end{figure}

Where

$\phi_0 :: a \rightarrow F_0\ a$

$\phi_1 :: a \rightarrow F_1\ a$

$\phi_2 :: b \rightarrow F_2\ b$

From which we obtain the system of equations:

$a = F_0\ a$

$b = F_1\ a$

$a = F_2\ b$

Which gives

$a = \mu(F_0 \circ F_2 \circ F_1)$

$b = F_1\ \mu(F_0\circ F_2 \circ F_1)$
