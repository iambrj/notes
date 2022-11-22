# Intro

- DBMS advantages over file system storage:
  + Concurrency
  + Independence between physical storage and logical organization
  + Efficient data access
  + Data integrity
  + Crash recovery
- 3-level schemas
  + External
  + Conceptual
  + Physical

# ER model

- ER model: entity, attributes, and relations
- Relationship : association between entities
- Descriptive attribute : attribute of relationship, rather than any involved
  entity
- Candidate key : set of attributes that uniquely identifies an entity
- Primary key : designated key from candidate keys
- Key constraint : entity with which we can uniquely determine the relationship
- One-to-one, one-to-many relationships
- Participation constraint:
  + Total participation constraint : every element in entity set must be in
    relationship
  + Partial participation constraint : not every element has to be
- Weak entity : entity without key (or with partial), has an identifying
  relationship with identifying owner. Weak entity must have total participation.
- Aggregation : relationship between relationships
- Bad choices in entity vs relationship leads to redundancy in storage. Use
  normalization.
- Ternary relationship vs aggregation

# Relational algebra

- selection
- projection
- union
- intersection
- set-difference
- cross-product

# SQL

- `LIKE` syntax
```
SELECT * FROM cd.facilities WHERE name LIKE '%Tennis%';
```
- `IN` syntax
```
SELECT * FROM cd.facilities WHERE facid IN (1, 5);
```
- `CASE` syntax
```
SELECT name, (CASE
                WHEN monthlymaintenance > 100
                THEN 'expensive'
                -- WHEN condition THEN result
                ELSE 'cheap' END) AS cost
FROM cd.facilities;
```
- timestamp comparison
```
SELECT memid, surname, firstname, joindate FROM cd.members WHERE joindate >= '2012-09-01 00:00:00';
```
- `DISTINCT`, `ORDER BY`, `LIMIT`
```
SELECT DISTINCT surname FROM cd.members ORDER BY surname LIMIT 10;
```
- `UNION` (also `INTERSECT`, `EXCEPT`)
```
SELECT surname FROM cd.members UNION SELECT name FROM cd.facilities;
```
- Aggregate functions (max, min, avg, sum)
```
SELECT MAX(joindate) AS LATEST
	FROM cd.members;
```
- Subquery
```
select firstname, surname, joindate
	from cd.members
	where joindate =
		(select max(joindate)
			from cd.members);
```
- Inner join
```
SELECT starttime FROM (cd.members JOIN cd.bookings ON cd.members.memid=cd.bookings.memid) WHERE firstname='David' and surname='Farrell';
```
- 
