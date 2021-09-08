---
author: Bharathi Ramana Joshi
title: 'Lectures notes for the Data Systems course, IIIT Hyderbad Monsoon 2021'
---
# Lecture 1, 16/08/2021
- Course topics
    + Memory management
    + Files, Indices, and Index implementation
    + Query Processing and Optimization
    + Transaction Management
    + Modern data systems
- Quizzes grading scheme: +10-5-3 (Marked correct option, marked incorrect
    option, unmarked correct option)
- 3 Level Schema
    1. External Schema
    2. Conceptual Schema
    3. Internal Schema
- Relational algebra: select, project, join
- Grading:
    + 40-50% project
    + 20-30% class exercises
    + 25-40% quizzes/exams

# Lecture 2
- Data system needs to manage its own memory, OS's default (general purpose)
    memory management is not ideal.

# Lecture 3
- Why? Some examples:
    1. Flushing dirty pages to disk.
    2. Prefetching for queries.
    3. Buffer management.
    4. Process scheduling.
- DB is going to manage
    * DB processes.
    * Chunk of disk data.
- Most DBMSs uses OS's file management system, rarely do DBMSs define their own
    file systems.
- DBMS page
    + Corresponding to OS page, fraction of OS page
- How is a relation stored in pages?
    + Relation -> File -> Page
    + Each row as a single unit ("tuple"), store each tuple in a page
    + Page: Header + tuple

# Tutorial 1, 17/08/2021
- 
