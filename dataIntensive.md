# Replication
- Many copies of data to scale horizontally and serve more reads
- Ways of replication
    1. Leader-follower: writes happen first on leader, then propagated to
       followers.
    2. Multi-leader
    3. Leaderless
- Types of replication:
    1. Synchronous: leader sends write and blocks writes till follower ack
    2. Asynchronous: leader sends write and continues with further writes
    3. Semi-synchronous: one follower is sync, rest async. If leader fails, the
       sync follower becomes leader.
## Leader-follower
- Setting up new followers: snapshot leader, launch follower, request changes
  since snapshot
- Node outage: catch up using on disk log
- Leader failover:
    1. Failover detection: polling. Poll frequency needs to account for load spike.
    2. New leader selection: election, controller node. Best candidate is with most writes.
    3. Reconfigure system to use new leader: old leader coming back causes split brain.
- Use manual failover to circumvent above subtleties.
- Replication log implementations: sequence of append-only bytes with all writes to db
    1. Statement-based: CRUD statements
    2. Write-Ahead Log (WAL) based: SStable/LSM Trees track sequence of bytes
       being edited in db. Tightly coupled with underlying storage engine.
    3. Logical (row) based: rows and columns affected.
    4. Trigger-based : execute custom application code (triggers & stored
       procedures) when changes to db occur.
- Eventual consistency : after a period of time, all data replicas will converge
  to the same state if no updates are made.
- Read-your-writes/Read-after-write consistency : users reads should be immediately visible.
- Monotonic reads: reader shouldn't read older reads after reading newer reads.
- Consistent prefix reads: reads must appear in the same order as writes.
## Multi-leader
- Multi-leader replication: write conflicts
- Conflict avoidance: write application code such that write conflicts do not
  occur. E.g. all writes of a user go through the same node.
- Converge writes:
    1. Last write wins (LWW)
    2. Prioritize nodes, higher priority node's write wins
    3. Merge conflicts
    4. Record conflict, fix on later read/write (possibly prompting user)
- Multi-leader replication topologies: how should writes propagate in nodes.
    1. All-to-all
    2. Circular
    3. Star/tree
## Leaderless
- Leaderless replication: reads and writes are sent in parallel to many nodes,
  quorum response is taken.
- Read repair: if different read values are returned by different nodes, correct
  the nodes with wrong read values.
- Anti-entropy process: have background process checking differences in data
  across nodes, copy missing data. No guarantees on data propagation delays.
- Common issue:
    * Sloppy-quorum: writes and reads ending up on different, non-overlapping nodes
    * Write conflicts
    * Concurrent reads and writes
    * Partially succeeded writes
- 
