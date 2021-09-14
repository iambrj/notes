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

# Lecture 2, 23/08/2021
- Data system needs to manage its own memory, OS's default (general purpose)
    memory management is not ideal.

# Lecture 3, 26/08/2021
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

# Lecture 4, 30/08/2021

# Lecture 5, 02/08/2021
- Relation -> File -> Page -> Tuples -> Attribute values.
- Pointers to locations for large binaries (image, pdf, etc).
- System catalog/metadata:
    1. DB Name, table, column, attribute domain type "Core DB metadata".
    2. Users & permissions.
    3. Internal statistics (no. of rows/cols etc).
- All attributes need NOT be stored in single page.
- n-ary storage model: tuple & its attributes in a single page.
    + Good for relations with fewer relations, accesses, and joins.
    + Bad for updates since multiple page accesses would be required.

# Lecture 5, 09/08/2021
- Unordered blocks/heap
    + Fast insert
    + Slow retrieve: (b / 2) on average (b = # of blocks)
    + Even slower for queries like attribute x > 5, for which entire heap needs
        to be scanned (b)
- Ordered blocks
    + Moderate insert (for sorted attribute)
    + Moderate retrieve (for sorted attribute): ceil(log2(b))
    + Moderate retrieve even for queries like attribute x > 5 (avg): ceil(log2(b)) + (b / 2)
    + No advantage for non-sorted attributes
    + Waste empty space
- Hashing, overflow blocks, chaining, multiple hash functions
- Dynamic hashing

# Tutorial 1, 17/08/2021
- 
