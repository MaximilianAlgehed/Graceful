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

$a = F_0\ a \times F_2\ b$

$b = F_1\ a$

Which gives

$a = F_0\ a \times (F_2 \circ F_1) a$

$b = F_1\ a$

In conclusion

$a = \mu F_a\ where\ F_a\ y = F_0\ y \times (F_2 \circ F_1)\ y$

$b = F_1\ \mu F_a$

Let's try to construct a catamorphism, to escape the fixpoint, from these types to our favourite type,
$X$.

$\bar{a_0} :: F_0 X \rightarrow X$

$\bar{a_2} :: F_2 X \rightarrow X$

$\bar{b_1} :: F_1 X \rightarrow X$

$\bar{bi} :: X \times X \rightarrow X$

$\bar{a_2} \circ F_2\ \bar{b_1} :: (F_2 \circ F_1)\ X \rightarrow X$

$\bar{bi} \circ \langle \bar{a_0},\ \bar{a_2} \circ F_2\ \bar{b_1} \rangle :: F_a\ X \rightarrow X$

Finally we can construct our catamorphism

$\llparenthesis \bar{bi} \circ \langle \bar{a_0},\ \bar{a_2} \circ F_2\ \bar{b_1} \rangle \rrparenthesis :: \mu F_a \rightarrow X$

Given which

$F_1\ \llparenthesis \bar{bi} \circ \langle \bar{a_0},\ \bar{a_2} \circ F_2\ \bar{b_1} \rangle \rrparenthesis :: F_1\ \mu F_a \rightarrow X$
