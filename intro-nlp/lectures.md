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
- Zipf distribution
