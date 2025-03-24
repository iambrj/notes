# 101
- Broker: a single Kafka server, which receives messages from producers,
  assigns offsets to them, and commits the messages to storage on disk.
- Cluster: set of brokers.
- Topic: collection of partitions
- Partition: single commit-log. Each partition can be hosted on a different
  server.
- Replication: multiple copies of partitions across brokers to improve
  fault-tolerance.
- More brokers => more time required to elect leader.
- Kafka is organized into topics, each topic has partitions, partitions have
  replicas stored on brokers.
- Each broker stores hundreds/thousands of replicas belonging to different
  topics and partitions.
- A partition is owned by a single broker in the cluster, and that broker is
  called the leader of the partition. All producer and consumer requests for a
  partition go through the partition's leader to ensure consistency.
- ZooKeeper is used by Kafka to store metadata about the Kafka cluster and the
  consumer client details.
- ZooKeeper is a centralized service for maintaining configuration information,
  naming, providing distributed synchronization, and providing group services.
  TODO explore ZooKeeper
- The key of a record determines which partition the record will be written to.
  All records with the same key are written to the same partition. Useful to
  distribute records among processes reading specific partitions.
- If a record's key is null, the record is written to a partition at random
  as determined by a round robin algorithm.
- As long as the number of partitions in a topic are not changed, records with
  the same key will be hashed to the same partition. If a new partition is
  added, the older records will not be moved but new records will get hashed
  according to the new number of partitions.
- Kafka's defualt partition algorithm is independent of the Java library
  version.

# Producers
- Trade-offs to consider when configuring Kafa:
  1. Is every message critical or can we tolerate loss of messages? Are we OK
     with duplicating messages?
  2. Are there any strict latency or throughput requirements we need to
     support?
- A separate thread is responsible for batching records to be sent to a
  particular topic and partition.
- If a broker is able to successfully write the messages, the broker returns a
  `RecordMetadata` object with the topic, partition, and the offset of the
  record within the partition; otherwise the broker returns an error.
- Three ways to use a producer:
  1. Fire-and-Forget
  2. Synchronously: get a future
  3. Asynchronously: pass a callback
- Producer configurations:
  1. 

# Consumers
- Consumer group: set of consumers of a topic in which each consumer receives
  records from a disjoint subset of partitions.
- If there are more consumers in a single group with a single topic than we have
  partitions, some of the consumers will be idle and get no messages at all.
