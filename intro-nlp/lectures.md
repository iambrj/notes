---
title : Lecture notes on Introduction to NLP
author : Bharathi Ramana Joshi
---

# 09/01/2021

- Lexicon : the complete set of meaningful units in a language.
- Token : actual word - cat, cats
- Type : word present in its root form in the dictionary. Ex : cat for cat and
    cats
- Vocabulary : set of all types
- Major families of languages in India : Dravidian and Aryan
- token / type high ratio => morphologically complex language. Ex : Sanskrit -
    highly
- tokenization : separating tokens from input
    Challenges
    * Numbers
        + The value of gravity is 9.8 m/s/s
        + He got 1,000,000 dollars in VC funding
        + My number is +918451990342
        + Take 1/2 cups of milk
    * Dates
        + My birth date is 03/09/1982
        + I joined on 22nd July
    * Abbreviation
        + Dr. S. P. Kishore is the primary faculty of this course
        + We are in IIIT-H campus
    * Punctuations
- Regular expressions
- N-grams : 
- Zipf's Law : 
- Foundations of natural language processing, zaraski

# 16/01/2021

- n-gram changes with corpus domain (think wikipedia text vs law text)
- Reliability vs discrimination, informativeness vs rarity
    + Larger n : greater discrimination, more information about context
    + Smaller n : more reliability, more instances in training data better
          estimate
- Higher n => need more space to store probabilities
- Major problems
    + How to deal with unknown words?
    + Data sparseness
- Given some words, what should the next word be?
- Perplexity & Entropy
    + Entropy : Given 2 LMs and a corpus, which LM is better watch for the
        corpus? Always want to choose one with lower entropy
    + Cross entropy
    + Perplexity

# 23/01/2021

- Good Turing estimate intuition : # of n-grams with count $(c + 1)$ is close to
    # of n-grams with count $c$

\begin{align*}
    c^* = (c + 1)\frac{N_{c+1}}{N_c}
\end{align*}
- Rescaling with count
    \begin{align*}
    P^*_{GT}(things with frequency zero in training) = \frac{N_1}{N}
    \end{align*}
    where

1. $N_1$ : # of items occurring once
2. $N$ : total # of items seen in training (note : not unique items, just items)

- Limitations
    1. Distribution of each bigram is binomial
    2. $\#$ of unknown $N_0$ is known

- Smoothing approaches
    1. Linear regression : $log(N_c) = a + b log(c)$
    2. For large counts, reliability is lost so $c^* = c$
- used in conjunction with backoff and interpolation
- Backoff use lower n gram after threshold frequency
- Interpolation use weighted interpolation
- Kneser-Ney Smoothing
    + Absolute discounting, subtract a fixed discount $d$ from each count
    + Use context to backoff
- Continuation probability

\begin{align*}
    P_{continuation}(w_i) = \frac{|\{w_{i - 1} : c(w_{i-1}w_i) > 0\}|}{\sum_{w_i}{|\{w_{i - 1} : c(w_{i-1}w_i) > 0\}|}}
\end{align*}

- Kneser-Ney : how likely is this word to continue with this context

\begin{align*}
    P_{KN}(w_i|w_{i - 1}) =
        \begin{cases}
        \frac{c(w_{i - 1}w_i) - D}{c(w_{i - 1})} & c(w_{i-1}w_i) > 0\\
        \alpha(w_i) \frac{|\{w_{i - 1} : c(w_{i-1}w_i) > 0\}|}{\sum_{w_i}{|\{w_{i - 1} : c(w_{i-1}w_i) > 0\}|}}, else
        \end{cases}
\end{align*}

- Caveats : $D$ value doesn't matter much, any small number works. $\alpha$
    matters a lot.

- Witten-Bell: how likely is this context to end with new word

    - Intuition: Smoothed probability $P_{WB}(w_i|w_{i-1:i-m})$ is higher if
        sequence $w_{i-1:i-m}$ occurs with many different words $w_i$

    \begin{align*}
    P_{WB}(w_i|w_{i - 1}) =
    \begin{cases}
    \frac{T(w_{i - 1})}{Z(w_{i - 1})(N(w_{i - 1}) + T(w_{i - 1}))} & c(w_{i-1}w_i) = 0\\
    \frac{c(w_{i - 1}w_i)}{N(w_{i - 1}) + T(w_{i - 1})} & c(w_{i-1}w_i) > 0\\
    \end{cases}
    \end{align*}
    where

1. $T(w_{i - 1})$ = # of unique types that occur to right of $w_{i - 1}$
2. $N(w_{i - 1})$ = total # of tokens to right of $w_{i - 1}$
3. $Z(w_{i - 1})$ = # of bigrams starting with $w_{i-1}$ not in training set

# 27/01/2021

- Mathematics of POS Tagging
