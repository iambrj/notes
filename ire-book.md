---
author: Bharathi Ramana Joshi
title: 'Notes made while reading "Introduction to Information Retrieval"'
---

# Chapter 1
- Boolean retrieval model: Queries that are boolean expressions of terms (i.e.
    terms combined with AND, OR, NOT).
- Inverted index: vocabulary of terms, and each term has a list of records for
    documents the term occurs in. Posting = each record in the list, postings
    list = list for a term. Postings = all lists together.

# Chapter 2
- Indexing granularity: break up single document into smaller documents? (e.g.
    email thread into individual emails).
- Precision/recall tradeoff: if units get too small, important passages may be
    missed because terms were distributed over several documents.
- Tokenizatoin
    1. Contractions (O'Neill, aren't)
    2. Proper nouns (C++, B-52, MASH)
    3. Email addresses
    4. Web URLS
    5. numeric IP addresses
    6. Hyphenation (over generalize, i.e. "over-eager" searches for "over eager" OR "over-eager" OR "overeager")
    7. Tokens with whitespace (San Fransisco, Los Angeles)
- Dropping common terms: stop words
    + Special queries may be made entirely of stop words (To be or not to be, I
        don't want to be, ...).
    + Can reduce index size.
- Normalization: canonicalizing tokens so that matches occur despite superficial
    differences in the character sequences of the tokens (e.g. USA and U.S.A.).
    + Build equivalence classes.
    + Handle synonyms by maintaining lists.
    + Accents and diacritics.
    + Case folding: mid-sentence capitalized words are left as capitalized.
        Machine learning via truecasing.
    + British/American spellings.
    + Stemming: chop off last few letters.
    + Lemmatization: recover dictionary root word. Produces at most modest
        benefits for retrieval.
## Skip pointers
- Skip pointers: shortcuts that allow us to avoid processing parts of the
    postings list that will not come up in the search results.
- Heuristic that works well in practise: place sqrt(P) evenly placed skip
    pointers for posting list of length P. This heuristic can be improved by
    exploiting details of distribution of query terms.
- Phrase query: "stanford university"
    + Biwords: treat two consecutive words as a phrase.
    + Positional indexes
