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

# Chapter 17 : Indexing Structures

- Indexing field/attribute: field/attribute on which the indexing structure is
  built
- Dense index: one index entry per search key value (and hence every record).
- Nondense/sparse index: index entries only for some of the search values.
- Primary index: index built from ordering key field of an ordered file
    + File of fixed length records of size 2: <key value, block address>
    + One index entry per block: has first record's (called the anchor
      record/block anchor) key value
    + Nondense index
- Clustering index: index built from (nonkey) ordering field, but multiple
  records may have same ordering field value. Such a data file is called a
  clustered file.
  + Nondense index
  + File of fixed length records of size 2: <key value, block address> to
    first block in data with that key value.
- Secondary index: index built from any nonordering field of a file.
  + Sorted one entry per value <key value, block address>
  + Dense index, doesn't use block anchors
  + Takes more space than primary index
- 

# Chapter 18 : Strategies for Query Processing
- Query block: single `SELECT-FROM-WHERE` expression, as well as `GROUP BY` and
  `HAVING` clauses
- `SEMI-JOIN`(`T1.X S = T2.Y`) : return row of `T1` as soon as `T1.X` finds a
  match with any value of `T2.Y` without further searching for further matches
  (unlike inner join, where all possible matches are found).
- `ANTI-JOIN`(`T1.X A = T2.Y`) : a row of `T1` is rejected as soon as `T1.X`
  finds a match with any value of `T2.Y` without further searching for further
  matches. Thus, only those rows from `T1` are selected that do not match
  against any value in `T2`.

## External/Out-of-core sorting
- Buffer space divided into *buffers*, where size of one buffer = size of one
  disk block.
- # initial runs (nR) = ceil(# blocks (n) / buffer space (nB))
- Degree of merging (dM) = min(nB - 1, nR)
- # of merge passes = ceil(logdM(nR))
- Merge-sort cost = 2 * b + (2 * b * (logdM(nR)))

## Implementing SELECT
- File/index scan
1. Linear scan: go through everything and check if each record satisfies the
   search criteria
2. Binary search: useful for equality selections
3. Primary index: can retrieve at most one record by using primary index for
   equality selections, or if file is sorted on indexing attribute first perform
   equality then retrieve all subsequent attributes
4. Hash: can retrieve at most one record by using primary index for
   equality selections
5. Clustering index: useful for selections on nonkey attributes
6. BPTree: leaves are ordered, can be used for equality and range queries
7. Bitmap index: useful for set of selection values, OR all their bitmaps
8. Functional index: match is based on function of one or more attributes on
   which a functional index exists
- Conjunctive selection
  1. Try to use index selections for any of the conjunction clauses, then apply
     rest of the conjunction clauses on these results.
  2. If composite index exists for conjunction clauses, use that.
  3. Set intersection: if indexes on multiple clauses pointing records are
     available, just take intersection
- Disjunctive selection: take union of clauses, can be sped up for those clauses
  that have access paths. If no access paths exist, the linear search must be
  performed.
- System catalog used to estimate costs contains:
  + For each relation r:
    1. # of rows/records rR
    2. Width of each row R
    3. # of blocks occupied by relation on disk bR
    4. Blocking factor bfr, # of tuples per block
  + For each attribute A in a relation R:
    1. NDV(A, R): # of distinct values of A in R
    2. max(A, R) and min(A, R)
- Selectivity: fraction in [0, 1] estimating how many of the relations are
  likely to be selected for this conjunction clause.
- Assuming a distribution (e.g. uniform), various estimates for selectivity can
  be made using above statistics, e.g.
  1. Equality on key attribute: 1 / |rR|
  2. Equality on nonkey attribute (with i distinct values)): 1 / i
  3. Range: max(A, R) - v / max(A, R) - min(A, R)

## Implementing PROJECT and Set operations
- To eliminate duplicates in PROJECT,
  1. Results can be sorted and consecutive kuplicates can be removed
  2. Hashing and checking if hashed value already exists in bucket
- Cartesian product is expensive so it must be substituted with some other
  operation (e.g. join)
- Union, intersection, and set difference can be implemented via sort-merging
- Set operations via hashing: 
- Set difference via anti-join

## Implementing aggregate operations
- MAX/MIN: search the index (e.g. BPTree keeping following right/left pointers)
- AVERAGE/SUM/COUNT: can be sped up using dense index, but all records must be
  fetched for nondense index.
- Grouping: sorting, hashing to form groups. If clustering index exists, they
    are already grouped.
