---
fontfamily: mathpazo
title: ''
author: 'Bharathi Ramana Joshi, 2019121006'
date: 'dd-mm-2021'
header-includes: |
    \hypersetup{colorlinks=true, urlcolor=blue}
---

# Chapter 1

- Rationality: each player acts to maximize their own utility
- Intelligence: each player has the capability to compute their best action
- Game Theory: the science of strategic interaction
- Applications of game theory in the modern world:
  1. Matching markets
  2. Sponsored search auctions
  3. Crowdsourcing mechanisms
  4. Social Network Analysis

# Chapter 2

- Strategic form game: $\langle N, (S_i)_{i\in N}, (u_i)_{i\in N}\rangle$, where
  1. $N = \{1,2,\dots,n\}$: set of players
  2. $S_i$ is strategy set of player $i$
  3. $u_i : S_1\times S_2\times\dots S_n\rightarrow\mathbb{R}$ : utility function
- Common knowledge: all players know a fact, (all players know that)^n all
  players know a fact
- Mutual knowledge: just all players know a fact, latter need not hold
- Perfect information: every player knows the history of moves of all players
- Complete information: no player has private information at the beginning of
  the game.

# Chapter 3

- Information set: set of states that are indistinguishable to an agent
- Extensive form game: $\langle N, (A_i)_{i\in N}, \mathbb{H}, P, (\mathbb{I})_{i\in N}, (u_i)_{i\in N}\rangle$ where
  1. $N = \{1,2,\dots,n\}$: set of players
  2. $A_i$: action set of player $i$
  3. $\mathbb{H}$: is the set of all terminal histories, where a terminal
     history is a path of actions from the root to a terminal node such that it
     is not a proper subhistory of any other terminal history. $S_ \mathbb{H}$
     denotes the set of all proper subhistories (including the empty history
     $\epsilon$) of all terminal histories
  4. $P:S_{\mathbb{H}}\rightarrow N$ is the \textbf{player function} associating
     each proper subhistory to a certain player
  5. $\mathbb{I}_i$ is the set of all information sets of player $i$. It forms a
     partition of vertices of player $i$
  6. $u_i:\mathbb{H}\rightarrow\mathbb{R}$ gives the utility of player $i$
     corresponding to each terminal history
- Perfect information: all information sets are singleton, players always know
    where they are in the tree, know how they reached there (i.e. subhistory is
    known)
- Strategy: $s_i : \mathbb{I}_i\rightarrow A_i$ such that $\forall
  J\in\mathbb{I}, s_i(J)\subseteq C(J)$ where $C(J)$ is the set of actions
  available to to player $i$ in the information set $J$ (i.e. $J\subseteq A_i$)

# Chapter 4

- Best response strategy: Given a strategic form game $\Gamma = \langle N,
  (S_i), (u_i)\rangle$, and a strategy profile $s_{-i}\in S_{-i}$, we say
  $s_i\in S_i$ is a best response strategy of player $i$ with respect to
  $s_{-i}$ if $u_i(s_i, s_{-i})\geq u_i(s_i', s_{-i}), \forall s_i'\in S_i$
- Zero-sum game: sum of utilities in all outcomes is zero.
- Common payoff games: payoffs are the same for both the players in all outcomes

# Chapter 5

- Strongly dominated strategy: $s_i'$ strongly dominates $s_i$ (or $s_i$ is
  strongly dominated by $s_i'$) if utility of $s_i'$ is strictly greater than
  utility of $s_i$, irrespective of the strategies of other players:
  \begin{align*}
  u_i(s_i', s_{-i}) > u_i(s_i, s_{-i}), \forall s_{-i}\in S_{-i}
  \end{align*}
- Strongly dominant strategy: $s_i^*$ is a strongly dominant strategy if it
  strongly dominates every other strategy available to player $i$.
  \begin{align*}
  u_i(s_i^*, s_{-i}) > u_i(s_i, s_{-i}), \forall s_{-i}\in S_{-i} \& \forall
  s_{i}\in S_{i}
  \end{align*}
- Strongly dominant strategy equilibrium: a strategy profile where the strategy
  of every player is a strongly dominant strategy
- Weakly dominated strategy: $s_i'$ weakly dominates $s_i$ (or $s_i$ is
  weakly dominated by $s_i'$) if:
  \begin{align*}
  u_i(s_i', s_{-i}) \geq u_i(s_i, s_{-i}), \forall s_{-i}\in S_{-i} \&\\
  \exists s_{-i}\in S_{-i}\textrm{ such that } u_i(s_i', s_{-i}) > u_i(s_i, s_{-i})
  \end{align*}
  i.e. strict inequality holds for at least one $s_{-i}$
- Weakly dominant strategy: $s_i^*$ is weakly dominant strategy if it weakly
  dominates every other strategy available to player $i$.
- Weakly dominant equilibrium: a strategy profile where the strategy of every
  player is a weakly dominant strategy.
- 

# Chapter 6

- Given a strategic form game $\Gamma = \langle N, (S_i), (u_i)\rangle$, the
  strategy profile $s^* = (s_1^*,\dots,s_n^*)$ is called a ***Pure Strategy Nash
  Equilibrium*** of $\Gamma$ if
  \begin{align*}
  u_i(s_i^*, s_{-i}^*) \geq u_i(s_i, s_{-i}^*), \forall s_i\in S_i
  \end{align*}
  Or, equivalently,
  \begin{align*}
  u_i(s_i^*, s_{-i}^*) = max_i u_i(s_i, s_{-i}^*), \forall s_i\in S_i
  \end{align*}
  In other words, each player's PSNE strategy is a best response to the Nash
  equilibrium strategies of all the other players.
- Best response correspondence: mapping $b_i : S_{-i}\rightarrow 2^{S_i}$
  defined as:
  \begin{align*}
  b_i(S_{-i}) = \{s_i\in S_i : u_i(s_i, s_{-i})\geq u_i(s_i', s_{-i}),
  \forall s_i'\in S_i\}
  \end{align*}
  Using this, the strategy profile $(s_1^*,\dots,s_n^*)$ 
- Any dominant strategy equilibrium is also a PSNE, other way need not hold.
- Interpretations of PSNE:
  1. Prescription, insurance against unilateral deviation
  2. Scientific prediction of game outcome
  3. Self-enforcing agreement
  4. Evolution and steady state
- Maxmin value: what is the minimum guaranteed payoff of player $i$, irrespective
  of what other players play?
  \begin{align*}
  \textrm{\underbar{$v_i$}} = \max_{s_i\in S_i} \min_{s_{-i}\in S_{-i}} u_i(s_i,
  s_{-i})
  \end{align*}
- Minmax value: what is the minimum payoff other players can force on player
  $i$, when they choose strategies that hurt player $i$ the most?
  \begin{align*}
  \bar{v_i} = \min_{s_{-i}\in S_{-i}} \max_{s_i\in S_i} u_i(s_i, s_{-i})
  \end{align*}
- PSNE payoff $\geq$ minmax $\geq$ maxmin

# Chapter 7

- Mixed strategy: probability distribution over strategies
- Joint probability of pure strategy is then defined as
  \begin{align*}
    \sigma(s_1,\dots,s_n) = \Pi_{i\in N}\sigma_i(s_i)
  \end{align*}
- The utilities are defined as
  \begin{align*}
    U_i(\sigma_1,\dots,\sigma_n) = \sum_{(s_1,\dots,s_n)\in S}\sigma(s_1,\dots,s_n)u_i(s_1,\dots,s_n)
  \end{align*}
- Mixed Strategy Nash Equilibrium: a profile $(\sigma_1^*,\dots,\sigma_n^*)$
  such that
  \begin{align*}
    u_i(\sigma_i^*, \sigma_{-i}^*)\geq u_i(\sigma_i, \sigma_{-i}^*), \forall
    \sigma_i\in \Delta (S_i)
  \end{align*}
- The payoff for any player under a mixed strategy can be computed as a convex
  combination of the payoffs obtained when the player plays pure strategies with
  the rest of the players playing $\sigma_{-i}$:
  \begin{align*}
  u_i(\sigma_i, \sigma_{-i}) = \sum_{s_i\in S_i} \sigma_i(s_i)u_i(s_i, \sigma_{-i})
  \end{align*}
  where
  \begin{align*}
  u_i(s_i, \sigma_{-i}) = \sum_{s_{-i}\in S_{-i}} (\prod_{j\neq i}\sigma_{-ij}(s_{-ij})) u_i(s_i, s_{-i})
  \end{align*}
- The highest payoff player $i$ can achieve using a mixed strategy is equal to
  the highest strategy he can achieve using a pure strategy:
  \begin{align*}
  \max_{\sigma_i\in\Delta(S_i)} u_i(\sigma_i, \sigma_{-i}) = \max_{s_i\in S_i} u_i(s_i)
  \end{align*}
  Furthermore, this is achieved iff probabilities of strategies not in
  $argmax_{s_i\in S_i}u_i(s_i, \sigma_{-i})$ is zero
- Support of a mixed strategy: set of strategies with nonzero probabilities.
  Denoted by $\delta(\sigma_i)$
- Support of a mixed strategy profile: cross product over players of set of strategies with nonzero probabilities
- A mixed strategy profile $(\sigma_1^*,\dots,\sigma_n^*)$ is a Nash equilibrium
  iff $\forall i\in N$
  1. $u_i(s_i, \delta_{-i}^*)$ is the same $\forall s_i\in\delta(\sigma_i^*)$
     and
  2. $u_i(s_i, \delta_{-i}^*)\geq u_i(s_i', \delta_{-i}^*)$, $\forall
     s_i'\not\in\delta(\sigma_i^*)$
The above conditions are necessary and sufficient.

Above theorem can be used to compute MSNE for the following game as follows:

![MSNE Computation example utilities](msne-compute-ex-u.png "MSNE Computation example utilities"){height=10%}

![MSNE Computation example](msne-compute-ex.png "MSNE computation example")

Implications:

  1. In MSNE, every player gets same payoff by playing any nonzero probability
     pure strategy
  2. To verify MSNE, it is enough to consider the effects of only pure strategy
     deviations
- For $s_i\in S_i$, $e(s_i)$ denotes the degenerate mixed strategy assigning 1
  to $s_i$ and 0 to all other strategies. Then, $(s_1^*,\dots,s_n^*)$ is a PSNE
  iff $(e(s_1^*),\dots,e(s_n^*))$ is a MSNE.

# Chapter 8

- Ordinal utilities: actual values don't matter, just relative order does
- A lottery is a probability distribution over outcomes. If $X =
    {x_1,\dots,x_n}$ is a set of outcomes, a lottery is:
    \begin{align*}
    [x_1:p_1,\dots,x_n:p_n]
    \end{align*}
    If there is uncertainty over outcomes, preferences can be over lotteries,
    instead of specific outcomes.
- Axioms of von Neumann-Morgenstern utility theory: these model the kinds of
    preferences players can have
  1. Completeness : $\forall x_1, x_2, x_1 > x_2,\ or x_2 > x_1,\ or x_1\sim x_2$
  2. Transitivity
  3. Substitutability/independence: if $x_1\sim x_2$, then they can be
     substituted for each other.
  4. Decomposability/simplification of lotteries: $P_{\sigma_1}(x_i) = P_{\sigma_2}(x_i) \forall x_i\implies \sigma_1\sim \sigma_2$
  5. Monotonicity: $x_1 > x_2\ \&\ 1\geq p > q \geq 0\implies [p : x_1, 1 - p : x_2] > [q : x_1, 1 - q : x_2]$
  6. Continuity: $x_1 > x_2\ \&\ x_2 > x_3\implies\exists p\in [0, 1]$ such
     that $x_2\sim [p : x_1, 1 - p : x_3]$
- von Neumann-Morgenstern theorem: given a set of outcomes $X = {x_1,\dots,x_n}$
  satisfying the von Neumann-Morgenstern axioms, there exists a utility function
  $u : X\rightarrow [0, 1]$ such that
  1. $u(x_1)\geq u(x_2)$ iff $x_1\geq x_2$
  2. $u([p_1:x_1;\dots;p_n:x_n]) = \sum_{j = 1}^m p_j u(x_j)$
- Because of the axioms, we can binary search between maximum preferred and
  minimum preferred outcomes to compute utilities of all outcomes.

# Chapter 9

- Saddle point: maximum in its column, minimum in its row.
- Value of matrix game in pure strategy $= v = \bar{v} = \underbar{v}$, if it
  exists.
- Saddle point, if it exists, is a PSNE.
- If $a_{ij}$ and $a_{hk}$ are both saddle points, then $a_{ik}$ and $a_{hj}$
  are also saddle points.
- All saddle points lead the same payoffs to all players
- Row player tries to maxmin, column player tries to minmax
- Row player's LP ($LP_1$):
  \begin{align*}
    &\textrm{maximize } z
    \textrm{ subject to }\\
    &z - \sum_{i = 1}^{m} a_{ij}x_i\leq 0, j = 1,\dots,n\\
    &\sum_{i = 1}^{m}x_i = 1
  \end{align*}
  Column player's LP ($LP_2$):
  \begin{align*}
    &\textrm{minimize } w
    \textrm{ subject to }\\
    &w - \sum_{j = 1}^{n} a_{ij}y_j\geq 0, i = 1,\dots,m\\
    &\sum_{j = 1}^{n}y_j = 1
  \end{align*}
- Minimax theorem: for $m\times n$ matrix game, there is a mixed strategy of row
  player $x* = (x_1*,\dots,x_n*)$ and a mixed strategy of column player $y* =
  (y_1*,\dots,y_n*)$ such that
  \begin{align*}
  \end{align*}

# Chapter 10

- Kakutani's Fixed Point Theorem: If $X\subset\mathbb{R}^n$ is a non-empty,
  compact, and convex subset of $\mathbb{R}^n$ and $f:X\rightarrow X$ is
  a correspondence such that
  1. $f$ is upper hemicontinuous
  2. $f(x)\subset X, \forall x\in X$ is non-empty and convex

  Then $f$ has a fixed point in $X$
- Nash equilibrium as fixed point of pure best response correspondence $b :
  S_1\times\dots\times S_n\rightarrow S_1\times\dots\times S_n$ and of mixed
  best response correspondence $b : \Delta(S_1)\times\dots\times
  \Delta(S_n)\rightarrow \Delta(S_1)\times\dots\times \Delta(S_n)$
- Sufficient conditions for the existence of PSNE
- Nash theorem : every finite game has a a MSNE

# Chapter 13

- Strategic form game with incomplete information: $\langle N, (\Theta_i),
  (S_i), (p_i), (u_i)\rangle$ where
  1. $(\Theta_i)$ : is the type of player $i$, which captures the private
    information this player has
  2. $(p_i)$ : is a mapping $\Theta_i\rightarrow \Delta(\Theta_{-i})$, capturing
    what player $i$ guesses other players' types are given $i's$ own type is
    $\theta_i$ as a probability distribution
- Consistency of beliefs: differences in beliefs can be explained by differences
  in information:
  \begin{align*}
  p_i(\theta_{-i} | \theta_i) = \frac{\mathbb{P}(\theta_i, \theta_{-i})}{\sum_{t_{-i}\in\Theta_{-i}} \mathbb{P}(\theta_i, t_{-i})}
  \end{align*}
  In other words, there is a prior probability distribution which can be
  conditionalized using Bayes' theorem upon player $i$'s type
- Strategy vs action in Bayesian game: strategy is a mapping
    $\Theta_i\rightarrow S_i$
- Selten game of the Bayesian game $\langle N, (\Theta_i),
    (S_i), (p_i), (u_i)\rangle$ is the equivalent strategic form game $\langle
    N^S, (S_{\theta_i})_{\theta_i\in\Theta_i \& i\in N}, (U_{\theta_i})_{\theta_i\in\Theta_i \& i\in N}) \rangle$
    Each player in the original Bayesian game is replaced with a type agent
    corresponding to each type in the typeset of the player

# Chapter 14

- Mechanism Design Environment
  1. $n$ rational \& intelligent agents $\{1,\dots,n\}$
  1. $X$ set of outcomes
  1. $\theta_i$ Type/private value of agent $i$
  1. $\Theta_i$ set of types/private values of agent $i$. Type profile is $(\theta_1,\dots,\theta_n)$
  1. $\mathbb{P}\in\Delta(\Theta)$ common prior
  1. $u_i : X\times\Theta_i\rightarrow\mathbb{R}$ utility function
  1. $X, N, \Theta_i, \mathbb{P}$ are assumed to be common knowledge
- Social Choice Function: $f:\Theta_1\times\dots\times\Theta_n\rightarrow X$,
  for given $N, \Theta_1,\dots,\Theta_n,X$
- Preference elicitation problem : problem of social planner getting agents'
  types
- Preference aggregation problem : computing outcome from reported types
- Direct mechanism: If $f$ is a social choice function, a direct (revelation)
  mechanism corresponding to $f$ consists of the tuple
  $(\Theta_1,\dots,\Theta_n,f(.))$
- Indirect mechanism : tuple $(S_1,\dots,S_n,g(.))$ where $S_i$ is the action
  set for agent $i$ and $g:S_1\times\dots\times S_n\rightarrow X$ maps action
  profiles to outcomes
- Bayesian game induced by a mechanism: the mechanism $\langle N, (\Theta_i), \mathbb{P}, X, u_i : X\times\theta_i\rightarrow \mathbb{R}\rangle$ induces the bayeisan game $\langle N, (\Theta_i), (S_i), (p_i), (U_i)\rangle$ where
\begin{align*}
U_i(\theta_1,\dots,\theta_n,s_1,\dots,s_n) = u_i(g(s_1,\dots,s_n), \theta_i)
\end{align*}
- Strategies in induced bayeisan game : what will player $i$ play was their type
  $\theta_i$? Denoted by $s_i(\theta_i)$ where $s_i:\Theta_i\rightarrow S_i$
- Implementation of SCF: a mechanism $\mathbb{M} = ((S_i)_{i\in N}, g(.))$ where
  $g:S_i\times\dots\times S_n\rightarrow X$ implements the social choice
  function $f(.)$ if there is a pure strategy equilibrium $s^*(.) =
  (s^*_1(.),\dots,s^*_n(.))$ of the Bayesian game induced by $\mathbb{M}$ such
  that $g(s^*_1(\theta_1),\dots,s^*_n(\theta_n)) = f(\theta_1,\dots,\theta_n),
  \forall (\theta_1,\dots,\theta_n)\in (\Theta_1,\dots,\Theta_n)$


# Chapter 15

- Implementability of SCFs
- Implementation in Dominant Strategy (DSI) : a mechanism $\mathbb{M} = ((S_i)_{i\in
  N}, g(.))$ where $g:S_i\times\dots\times S_n\rightarrow X$ implements the
  social choice function $f(.)$ in Dominant Strategy if there is a weakly
  dominant strategy equilibrium $s^*(.) = (s^*_1(.),\dots,s^*_n(.))$ in the
  Bayesian game induced by $\mathbb{M}$ such that
  $g(s^*_1(\theta_1),\dots,s^*_n(\theta_n)) = f(\theta_1,\dots,\theta_n),
  \forall (\theta_1,\dots,\theta_n)\in (\Theta_1,\dots,\Theta_n)$
- Implementation in Bayesian Nash Equilibrium (BNI) : a mechanism $\mathbb{M} = ((S_i)_{i\in
  N}, g(.))$ where $g:S_i\times\dots\times S_n\rightarrow X$ implements the
  social choice function $f(.)$ in Bayesian Nash equilibrium if there is a pure strategy
  Bayesian Nash equilibrium $s^*(.) = (s^*_1(.),\dots,s^*_n(.))$ in the Bayesian game induced
  by $\mathbb{M}$ such that $g(s^*_1(\theta_1),\dots,s^*_n(\theta_n)) =
  f(\theta_1,\dots,\theta_n), \forall (\theta_1,\dots,\theta_n)\in
  (\Theta_1,\dots,\Theta_n)$

# Chapter 16

- Dominant Strategy Incentive Compatibility (DSIC): A SCF $f:
  \Theta_1\times\dots\times\Theta_n\rightarrow X$ is said to be Dominant
  Strategy Incentive Compatible if the direct revelation mechanism $\mathbb{D} =
  ((\Theta_i)_{i\in N}, f)$ has a weakly dominant strategy equilibrium $s^* =
  (s^*_1(.),\dots,s^*_n(.))$ in which $s^*_i(\theta_i) = \theta_i, \forall i\in
  N$
- Bayesian Incentive Compatibility (BIC): A SCF $f:
  \Theta_1\times\dots\times\Theta_n\rightarrow X$ is said to be Bayesian
  Incentive Compatible if the direct revelation mechanism $\mathbb{D} =
  ((\Theta_i)_{i\in N}, f)$ has a Bayesian Nash equilibrium $s^* =
  (s^*_1(.),\dots,s^*_n(.))$ in which $s^*_i(\theta_i) = \theta_i, \forall
  i\in N$
- Revelation principle: illustrates the relationship between an indirect
  implementation and the direct implementation of a social choice function.
- Ex-Post Efficiency: no individual can be better off without making at least
  one individual worse off. Mathematically, a social choice function $f :
  \Theta\rightarrow X$ is EPE if for every type profile $\theta\in\Theta$, the
  outcome $f(\theta)$ is Pareto optimal. An outcome $f(\theta)$ is Pareto
  optimal if there is outcome $x\in X$ such that
  \begin{align*}
       &u_i(x, \theta_i)\geq u_i(f(\theta), \theta_i), \forall i\in N, \\
    \&\ &u_i(x, \theta_i) > u_i(f(\theta), \theta_i), \textrm{ for some } i\in N, \\
  \end{align*}
- Dictator is an individual for whom all outcomes of the SCF turn out to be
  most favored outcomes.
- A preference relation on a set of choices is \textbf{rational} if it is
  reflexive, transitive and complete.
- Properties of rational preference relations:
  1. Any preference relation induced by a utility function $u_i(.,\theta_i)$ is
     rational.
  2. For any finite set, every rational preference relation on it has infinitely
     many utility functions inducing it.
- Strict-Total Preference Relation: Rational preference relation that is also
  antisymmetric
- Gibbard-Satterthwaite Impossibility Theorem: A SCF $f:\Theta\rightarrow X$
  such that
  1. $X$ is finite and $|X|\geq 3$
  2. $\mathbb{R}_i = \mathbb{P}$, $\forall i\in N$
  3. $f$ is onto, i.e. all outcomes are possible
  then $f$ is DSIC iff it is dictatorial

# Questions
- Why does Nash equilibrium guarantee at least maxmin value?
