# Replication
- Many copies of data to scale horizontally and serve more reads
- Ways of replication
    1. Leader-follower: writes happen first on leader, then propogated to
       followers.
    2. Multi-leader
    3. Leaderless
- Types of replication:
    1. Synchronous: leader sends write and blocks writes till follower ack
    2. Asynchronous: leader sends write and continues with further writes
    3. Semi-synchronous: one follower is sync, rest async. if leader fails, the
       sync follower becomes leader.
- Setting up new followers: snapshot leader, launch follower, request changes
  since snapshot
- Node outage: catch up using on disk log
- Leader failover:
    1. 
