---
title: "Notes on : Speech and Language Processing"
author : Bharathi Ramana Joshi
---

# Regular Expressions, Text Normalization, Edit Distance

- Text normalization: converting text to a more convenient, standard form
- Tokenization : separating out words from running text
- Lemmatization : determining whether two words have same root
- Stemming : stripping suffixes
- Sentence segmentation : breaking at comma, period etc
- Clitic : contractions marked by apostrophes *what're*

# N-Grams

- Language Models - models that assign probabilities to sequences of words
- Naive approach

    \begin{gather*}
    P(the|its\; water\; is\; so\; transparent\; that\; the) =
    \frac{C(its\; water\; is\; so\; transparent\; that\; the)}{C(its\; water\; is\; so\; transparent\; that)}
    \end{gather*}

Does not work because

1. Need large corpus to collect useful frequencies
2. Not malleable even to small changes in sentences
3. Sparse matrix problems

- Formalizing above intuition using chain rule,

    \begin{align*}
    P(w_{1:n}) &= P(w_1)P(w_2\mid w_1)\dots P(w_n\mid w_{1:n - 1})\\
               &= \Pi_{k = 1}^{n}P(w_k\mid w_{1: k - 1})
    \end{align*}

- Approximating to only most recent 1 word, 2 words etc we end up with bigram,
    trigram etc models

## Evaluating language models

- Extrinsic evaluation : embed model into application and see how much
    application improves
- Intrinsic evaluation : measures quality independent of application
- Perplexity : inverse probability of the test set. Intuition : weighted average
    branching factor

    \begin{align*}
    \sqrt[N]{\Pi_{i = 1}^{N}\frac{1}{P(w_i\mid w_1\dots w_{i - 1})}}
    \end{align*}

    Note:

    \begin{enumerate}
    \item Test set **must** be hidden from training set, otherwise we end up
    with an artificially low perplexity
    \item Perplexities of two models are comparable only if they are trained on
    the same vocabulary
    \end{enumerate}

## Generalization and Zeroes

- Closed vocabulary : guarantee that there are no unknown words
- Open vocabulary : handles unknown words by adding a pseudo-word `<UNK>`
- Out Of Vocabulary (OOV) rate : percentage of words that appear in the test
    set, but not training set
- Replace `<UNK>` with some other word with less probability

# Smoothing

- Smoothing/discounting : removing probability from some high frequence words to
    reassign to words appearing in unseen contexts
- Laplace smoothing : add 1 to everything to get rid of nonzero grams
- Add-k : instead of 1, add some k to all unseen words
- Backoff : Decrease $n$ and see if there are any apperances
- Interpolation : add weights instead of compeletly ignoring. For instance, for
    linear interpolation
    \begin{align*}
    P(w_n\mid w_{n - 2}w_{n - 1}) &= \lambda_1(w_{n-2:n-1}P(w_n|w_{n - 2}w_{n - 1})) \\
                                  &+ \lambda_2(w_{n-2:n-1}P(w_n|w_{n - 1})) \\
                                  &+ \lambda_3(w_{n-2:n-1}P(w_n))
    \end{align*}
    such that $\sum_1^3 \lambda_i = 1$
- Kneser-Ney Smoothing
    \begin{align*}
    P_{KN}(w_i|w^{i - 1}_{i - n + 1}) = \frac{max(c_{KN}(w^i_{i-n+1}) - d, 0)}
        {c_{KN}(w^{i - 1}_{i - n + 1})} + \lambda(w^{i - 1}_{i - n + 1})
        P_{KN}(w_i\mid w^{i - 1}_{i - n + 2})
    \end{align*}
    where
    \begin{align*}
    c_{KN}(\cdot) =
        \begin{cases}
        count(\cdot) & \textrm{for highest order}\\
        continuationcount(\cdot) & \textrm{for lower order}
        \end{cases}
    \end{align*}
    and
    \begin{align*}
    \lambda(\cdot) = \frac{d}{c(.)}|\{w:c(\cdot,w) > 0\}|
    \end{align*}

# Logistic regression

- Two approaches given observable $X$ and target $Y$
    + Generative : statistical model of joint probability distribution $P(X,
        Y)$. E.g. Naive Bayes
    + Discriminative : statistical model of conditional probability
      $P(Y | X = x)$. E.g. Logistic regression
- Components of a classification ML system
    1. Feature representation : $[x_1,\dots,x_n]$
    2. Classification function : $p(y|x)$ (sigmoid, softmax)
    3. Objective function : cross-entropy loss function
    4. Optimizing objective function : stochastic gradient descent

## Sigmoid

- Defined by
\begin{align*}
    y = \sigma(z) = \frac{1}{1 + e^{-z}} = \frac{1}{1 + exp(-z)}
\end{align*}
- Graph looks like "S" minimizing at 0 and maximizing at 1
- Features designed by examining the training set with linguistic intuitions and
    linguistic literature on the domain
- Representation learning : ways to automatically learn features in an
    unsupervised way from the input
- Naive Bayes vs Logistic Regression
    + Naive Bayes gives low accuracy probabilities
    + Makes strong independence assumptions, which means correlations cannot be
        exploited
    + Faster to implement and very fast to train. Well suited for smaller
        documents
    + Logistic regression exploits correlations
    + Better suited for larger documents
- Sigmoid is useful for expression classification probabilities precisely
    because it sums up to 1. Say, for binary classification, it can used as
    follows

    \begin{align*}
    P(y = 1) &= \sigma(w.x+b)\\
             &= \frac{1}{1 + exp(-(w.x + b))}\\
    P(y = 0) &= 1 - \sigma(w.x+b)\\
             &= 1 - \frac{1}{1 + exp(-(w.x + b))}\\
             &= \frac{exp(-(w.x+b))}{1 + exp(-(w.x + b))}\\
    \end{align*}

## Cross-entropy loss function

- Cross-entropy loss function

    \begin{align*}
    L_{CE}(\hat{y}, y) = -logp(y|x) = -[ylog\hat{h} + (1 - y)log(1 - \hat{y})]
    \end{align*}

## Gradient Descent

- Goal

    \begin{align*}
    \hat\theta = argmin_\theta\frac{1}{m}\sum_{i =
    1}^{m}L_{CE}(f(x^{(i)};\theta), y^{(i)})
    \end{align*}

- Gradient

    \begin{align*}
        \nabla_\theta L(f(x;\theta), y) =
        \begin{bmatrix}
            \frac{\partial}{\partial w_1}L(f(x;\theta), y) \\
            \frac{\partial}{\partial w_2}L(f(x;\theta), y) \\
            \dots \\
            \frac{\partial}{\partial w_n}L(f(x;\theta), y)
        \end{bmatrix}
    \end{align*}

    i.e.

    \begin{align*}
    \theta_{t + 1} = \theta_t - \eta\nabla L(f(x;\theta), y)
    \end{align*}

## Regularization

- Add a regularization term, $R(\theta)$ to penalize large weights

    \begin{align*}
    \hat\theta = argmax_\theta \sum_{i = 1}^{m}log P(y^{(i)}|x^{(i)}) -
    \alpha R(\theta)
    \end{align*}

- Euclidean distance/L2 regularization/Lasso regression

    \begin{align*}
    R(\theta) = ||\theta||^2_2 = \sum_{j = 1}^{n}\theta^2_j
    \end{align*}

- Manhattan distance/L1 regularization/Ridge regression

    \begin{align*}
    \hat\theta = argmax_{\theta}[\sum_{i = 1}^m logP(y^{(i)}|x^{(i)})] -
    \alpha\sum_{j = 1}^{n}\theta_j^2
    \end{align*}

## Multinomial logistic regression

- Softmax function, generalization of sigmoid to multiple dimensions

    \begin{align*}
    softmax(z_i) = \frac{exp(z_i)}{\sum_{j = 1}^{k}exp(z_j)}, 1\leq i\leq k
    \end{align*}

- Negative log likelihood loss

    \begin{align*}
    L_{CE}(\hat{y}, y) &= -log\hat{y}_k\\
                       &= -log\frac{exp(w_k.x + b_k)}{\sum_{j =
                       1}^{k}exp(w_j.x+b_j)}
    \end{align*}

# Sequence Labeling for Parts of Speech and Named Entities

- Named entity : anything that can be referred to with a proper name (person,
    location, organization etc)
- Sequence labelling : for each input word $x_i$ assigning a label $y_i$ such
  that both input and assigned sequences have same length
- Closed class parts of speech : relatively fixed membership (prepositions).
    A.k.a. function words (of, it, and, you etc). Very short, occur very
    frequently, have structuring use in grammar
- Open class parts of speech : nouns, verbs (iPhone, fax)

## English grammar 101

- Nouns : words for people, places, things
- Common nouns : concrete terms like cat and mang, abstractions like algorithm
    and beauty
- Count noun : occur in singular and plural (goat/goats,
    relationship/relationships)
- Mass noun : for homogeneous group (snow, salt, communism)
- Proper noun : names of specific persons/entities (Bharath, Lenovo etc)
- Verb : words for actions and processes (draw, provide, go)
- Adjectives : words describing nouns (good, bad, old, young)
- Adverb : modify other adverbs, verb phrases etc
- Directional/locative adverbs : home, here, downhill
- Degree adverbs : extremely, very, somewhat
- Manner adverbs : slowly, delicately
- Temporal adverbs : yesterday, Monday
- Interjections : oh, hey, alas, uh, um
- Prepositions : words expressing spatial/temporal relations occurring before
    nouns (on it, before then, on time, with gusto)
- Particle : preposition/adverb used in combination with a verb
- Phrasal verb : verb + particle acting as a single unit
- Determiners : this, that etc marking the start of an English noun phrase
- Article : a, an, the; determiners that mark discourse properties of noun
- Conjunction : join two phrases, clauses or sentences
- Pronoun : shorthand for referring to an entity or event
- Personal pronouns : refer to persons/entities (you, she, I, it)
- Possessive pronouns : personal pronouns that indicate either actual possession
    or abstract relation between person and some object
- Wh-pronouns (what, who, whom, whoever)
- Auxiliary verbs : mark semantic features of a main verb such as its tense (is
    it completed, is it negated, is it necessary, possible, suggested or
    desired). E.g. be, do, have

## POS tagging

- POS tagging as a disambiguation task : role of POS is to resolve ambiguities
- Always compare an algorithm with most frequent class baseline
- BIO tagging :
    1. B : begins a span of interest
    2. I : inside a span of interest
    3. O : Outside any span of interest
- BIOES : BIO + End + single span

## HMMs

- Hidden Markov Model (HMM)
    1. $Q = q_1,\dots,q_n$ set of $N$ states
    2. $A[n\times n]$ transition probability matrix
    3. $O[T]$ observations drawn from vocabulary $v_1\dots v_V$
    4. $B = b_i(o_t)$ observation likelihoods -- probability of observing $o_t$
       when in state $q_i$
    5. $\pi = \pi_1\dots \pi_N$ initial probability distribution
- First order hidden Markov Model
    1. Markov assumption $P(q_i|q_1,\dots,q_{i - 1}) = P(q_i|q_{i - 1})$
    2. Output Independence $P(o_i |
       q_1,\dots,q_i,\dots,q_T,\dots,o_1,\dots,o_i,\dots,o_T) = P(o_i | q_i)$
- Components of HMM
    1. $A$ matrix : tag transition probabilities
    \begin{align*}
    P(t_i|t_{i - 1}) = \frac{C(t_{i - 1}, t_i)}{C(t_{i - 1})}
    \end{align*}
    2. $B$ matrix : emission probabilities
    \begin{align*}
    P(w_i|t_i) = \frac{C(t_i, w_i)}{C(t_i)}
    \end{align*}
- HMM tagging as decoding : given as input a HMM $\lambda = (A, B)$ and a
    sequence of observations $O = o_1,o_2,\dots,o_T$ find the most probable
    sequence of states $Q = q_1q_2\dots q_T$
\begin{align*}
    \hat{t}_{1:n} &= argmax_{t_1\dots t_n}P(t_1\dots t_n | w_1\dots w_n)\\
                  &= argmax_{t_1\dots t_n}\frac{P(w_1\dots w_n | t_1\dots t_n)
                      P(t_1\dots t_n)}{P(w_1\dots w_n)}\textrm{(Baye's rule)}\\
                  &= argmax_{t_1\dots t_n}P(w_1\dots w_n | t_1\dots t_n)
                      P(t_1\dots t_n)
\end{align*}

Two further assumptions by HMM taggers

1. $P(w_1\dots w_n | t_1\dots t_n)\approx\prod_{i=1}{n}P(w_i|t_i)$, i.e.
   probability of word depends only on its tag and is independent of
   neighbouring tags and words
2. $P(t_1\dots t_n)\approx\prod_{i = 1}{n} P(t_i|t_{i - 1})$ i.e. bigram
   assumption : probability of a tag is dependent only on the previous tag,
   rather than entire sequence

Therefore,
\begin{align*}
    \hat{t}_{1:n} &= argmax_{t_1\dots t_n}P(w_1\dots w_n | t_1\dots t_n)
                      P(t_1\dots t_n)\\
                  &= argmax_{t_1\dots t_n}\prod_{i = 1}^{n} P(w_i|t_i)P(t_i|t_{i
                  - 1})
\end{align*}

i.e. emission $\times$ transition

- Viterbi algorithm : observation sequence $\times (A, B)\rightarrow$ best tag
      sequence, tag sequence prob
