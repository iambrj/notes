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

# Questions

- Why should perplexities be compared across models with same vocabulary?
