A game of life and death
========================

Consider the graph

\usetikzlibrary{graphs}
\usetikzlibrary{arrows}
\begin{figure}[H]
\centering
\begin{tikzpicture}[->, thick, auto, nd/.style={draw, circle, node distance=2cm, minimum width=2em}]
    \node[nd] (a) {\Large{$population$}};
    \path (a) edge[loop right] node [right] {\Large{$births$}} (a);
    \path (a) edge[loop left] node [left] {\Large{$deaths$}} (a);
\end{tikzpicture}
\end{figure}

Where

$births :: a -> Births\ a$

$deaths :: a -> Deaths\ a$

From which we obtain

$a = Births\ a \times Deaths\ a$

$a = \mu (\lambda y \rightarrow\ Births\ y \times Deaths\ y)$

Now the choice of functors $Births$ and $Deaths$ becomes relevant.
Let's take the rather arbitrary $Births\ A = Int\times Int$ and likewise $Deaths\ A = Int\times A$.
Where $Births$ represents the number of people in the generation, and the number of people born,
while $Deaths$ represents the number of deaths in the population.
We obtain

$a = \mu (\lambda y \rightarrow\ Births\ y \times Deaths\ y) =\\
 \mu (\lambda y \rightarrow Int \times Int \times Int \times y)$

Say that in each generation $10\times\log{population}$ individuals are born, 
and $15\%$ of the population dies.
From this we get

$births\ past@(living,\ born,\ dead,\ \_) =\\
 (living+10\times\log{living},\ 10\times\log{living},\ dead,\ past)$

$deaths\ past@(living,\ born,\ dead,\ \_) = (living*0.85,\ born,\ dead*0.15,\ past)$

It is clear that with these coalgebras ($births$ and $deaths$) and an initial condition like

$initialCondition\ population = (population,\ 0,\ 0,\ initialCondition\ population)$

we get functions

$generation = births \circ deaths$

and

$generations\ p\ 0 = initialCondition\ p\\
 generations\ p\ n = generation\ (generations\ p\ (n-1))$.

Something to note here is that since $a$ is a stream we can define
$births$ and $deaths$ functions that take in to account the age of individuals
in the population etc. If one were bold one could even define
$Births\ A = (Int + 1)\times Int$ to denote the beginning of the process.
