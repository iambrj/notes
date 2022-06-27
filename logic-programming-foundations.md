# Prelims
- First order theory:
    1. Alphabet
    2. First order language (well formed formulas of the theory)
    3. Set of Axioms (designated subset of well formed formulas)
    4. Set of Inference Rules
- Use axioms and inference rules to derive theorems of theory
- Alphabet
    1. Variables (x, y, z, ... informally)
    2. Constants (a, b, c, ... informally)
    3. Function symbols (f, g, h, ... informally)
    4. Predicate symbols (p, q, r, ... informally)
    5. Connectives (~, ∨, ∧, →, ↔)
    6. Quantifiers (∀, ∃)
    7. Punctuation symbols ("(", ")", ",")
- Precedence: (~, ∀, ∃) > (∨) > (∧) > (→, ↔)
- A term is inductively defined as:
    1. A variable is a term.
    2. A constant is a term.
    3. If f is a n-ary function symbol and t1, ..., tn are terms, then
    f(t1, ..., tn) is a term.
- A well formed formula is inductively defined as:
    1. If p is a n-ary predicate symbol and t1, ...., tn are terms, then
    p(t1, ..., tn) is a formula (aka atom, atomic formula).
    2. If F and G are formulae, so are (~F), (F ∨ G), (F ∧ G), (F → G), (F ↔ G).
    3. If F is a formula and x is a variable, so are (∀x F), (∃x F).
- Scope of ∀x in ∀x F is F. A bound occurrence of a variable in a formula is an
  occurrence immediately following a quantifier or an occurrence within the
  scope of the quantifier, which has the same variable immediately after the
  quantifier. Any other occurrence of a variable is free.
- Closed formula : formula with no free occurrence of any variables
- Universal closure of a formula F, ∀ F, is obtained by universally quantifying
  all free variables in F. Existential closure of a formula F, ∃ F, is obtained
  by existentially quantifying all free variables in F.
- Positively/Negatively occurring atom:
    1. An atom A occurs positively in A
    2. If atom A occurs positively (resp., negatively) in a formula W, then A
       occurs positively (resp., negatively) in ∃ W, ∀ W, and W ∨ V, W ∧ V, and W ← V
    3. If atom A occurs positively (resp., negatively) in a formula W, then A
       occurs negatively (resp., positively) in ~W and V ← W
- A **literal** is an atom or the negation of an atom. A positive literal is an
  atom, a negative literal is the negation of an atom.
- A **clause** is a formula of the form ∀x1...∀xs (L1 ∨ ... ∨ Lm), where Li
  are literals and x1...xs are all the variables occurring in all (L1 ∨ ... ∨
  Lm).
- Convenient notation: We denote ∀x1...∀xs (A1 ∨ ... ∨ Ak ∨ ~B1 ∨ ... ∨ ~Bn)
  where A1, ...  Ak, B1, ... Bk are all atoms and x1 ... xs are all variables
  occurring in (A1 ∨ ... ∨ Ak ∨ ~B1 ∨ ... ∨ ~Bn) by A1...Ak ← B1 ... Bn where
  Ai are connected by disjunction and Bi are connected by conjunction.
- A **definite program clause** is a clause of the form A ← B1 ... Bn (for all
  variable assignments, if all Bi hold then A holds).
- A **unit clause** is of the form A ← (for all variable assignments, A holds).
- A **definite program** is a finite set of definite program clauses.
- In a definite program, the set of all definite program clauses with the same
  predicate symbol p in the head is called the definition of p.
- A **definite goal** is a clause of the form ← B1 ... Bn
- **Empty clause** is with empty precedent and empty consequent
- A **Horn clause** is a clause which is either a definite program clause or
  definite goal
- Interpretation:
  1. Domain of discourse over which the variables range
  2. Assignment to each constant an element from the domain
  3. Assignment to each function symbol a mapping over the domain
  4. Assignment to each predicate symbol a relation over the domain
- Model of a formula = interpretation in which the formula is true
- Pre-interpretation of a first order language L consists of:
  1. A non-empty set D, the domain of pre-interpretation.
  2. For each constant in L, the assignment of an element in D.
  3. For each n-ary function symbol in L, the assignment of a mapping from D^n
     to D.
- An interpretation I of a first order language L consists of a
  pre-interpretation J with domain D of L together with:
  1. For each n-ary predicate symbol in L, the assignment of a mapping from D^n
     to {true, false} (or equivalently, a relation in D^n).
- If J is a pre-interpretation of a first order language L, a variable
  assignment (wrt J) is an assignment to each variable in L of an element in the
  domain of J.
- If J is a pre-interpretation of a first order language L, V a variable
  assignment wrt J then the term assignment (wrt V and J) of the terms in L is
  defined as follows:
  1. Each variable is given its assignment according to V.
  2. Each variable is given its assignment according to J.
  3. If t1',...,tn' are the term assignments of t1,...,tn and f' is the
     assignment of the n-ary function symbol f, then f'(t1',...,tn') ∈ D is the
     term assignment of f(t1,...,tn).
- If J is a pre-interpretation of a first order language L, V a variable
  assignment wrt J, and A an atom say p(t1...tn) and d1...dn wrt are the term
  assignments of t1...tn. A_{J, V} = p(d1...dn) is called the J-instance of A
  wrt V.
- If I is an interpretation with domain D of a first order language L and let V
  be a variable assignment. Then a formula in L can be given a truth value, true
  or false, (wrt I or V) as follows:
  1. If L is an atom p(t1...tn) check if the interpretation is true.
  2. If L is a formula of the form ~F, (F ∨ G), (F ∧ G), (F → G), (F ↔ G); then
     use truth tables.
  3. If L is a formula of the form ∃x F, then the truth value of the formula is
     true if there exists a d ∈ D such that F has truth value wrt I and V[x/d]
     where V[x/d] is v except that x is assigned d; otherwise, its truth value
     is false.
  4. If L is a formula of the form ∀x F, then the truth value of the formula is
     true if for all d ∈ D we have that F has a truth value wrt I and V[x/d]
     where V[x/d] is v except that x is assigned d; otherwise, its truth value
     is false.
- If I is an interpretation for a first order language L and W a formula in L:
  1. W is satisfiable in I if ∃(W) is true wrt I.
  2. W is valid in I if ∀(W) is true wrt I.
  3. W is unsatisfiable in I if ∃(W) is false wrt I.
  4. W is nonvalid in I if ∀(W) is false wrt I.
- If I is an interpretation of a first order language L and F a closed formula
  of L, then I is a model for F if F is true wrt I.
- The axioms of a first order theory are a designated susbset of closed formulas
  in the language of the theory.
- If T is a first order theory and L the language of T, a model for T is an
  interpretation for L which is a model for each axiom of T.
- If S is a set of closed formulas of a first order language L and I an
  interpretation of L. I is a model for S if I is a model for each formula of
  S.
- If S is a set of closed formulas of a first order language L:
  1. S is satisfiable in L if L has an interpretation which is a model for S.
  2. S is valid in L if every interpretation of L is a model for S.
  3. S is unsatisfiable in L if no interpretation of L is a model for S.
  4. S is nonvalid in L if L has an interpretation that is not a model for S.
- If S is a set of closed formulas and F a closed formula of a first order
  language L, then F is a logical consequence of S if for every interpretation
  I of L, I is a model for S implies that I is a model for F.
- If S is a set of closed formulas and F a closed formula of a first order
  language L, then F is a logical consequence of S iff S ∪ {~F} is
  unsatisfiable.
- A ground term is a term without any variables.
- A ground atom is an atom without any variables.
- The **Herbrand Universe U_L** for a first order language L is the set of all
  ground terms that can be formed out of constants and function symbols
  appearing in L.
- The **Herbrand Base B_L** for a first order langauge L is the set of all ground
  atoms that can be formed out of predicate symbols from L with ground terms
  from the Herbrand universe as the arguments.
- If L is a first order language, the Herbrand pre-interpretation for L is the
  pre-interpretation given by the following:
  1. The domain of the pre-interpretation is the Herbrand universe U_L.
  2. Constants in L are assigned to themselves in U_L.
  3. If f is a n-ary function symbol in L, then the mapping from (U_L)^n into
     U_L defined by (t1...tn) -> f(t1...tn) is assigned to f.
- Every Herbrand Interpretation is identified by a subset of the Herbrand base.
- If S is any set of closed formulas of first order language L, an Herbrand
  model for S is an Herbrand interpretation for L which is a model for S.
- If S is a set of clauses and it has a model, then S also has an Herbrand
  model.
- If S is a set of clauses, then S is unsatisfiable iff S has no Herbrand
  models.
- Note that S is a set of clauses, not arbitrary fomulas --- it is generally not
  possible to show S is unsatisfiable by restricting attention to Herbrand
 interpretations.
