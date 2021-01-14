---
title : "Notes on - Distributed Computing : Principles, Algorithms, and Systems"
author : Bharathi Ramana Joshi
---

# Introduction

- A distributed system is one in which the failure of a computer you didn't even
    know existed can render your own computer unusable.
- A distributed system is a collection of independent entities that cooperate to
    solve a problem that cannot be individually solved
- Distinctive features of a distributed system
    1. No common physical clock - introduces asynchrony
    2. No shared memory - use message passing
    3. Geographical separation - Google based on Network Of Workstations (NOW)
    4. Autonomy and heterogeneity - loosely coupled, since computers have
       different speeds and can be running different operating systems
- Middleware : Software that drives the distributed system to perform a
    computation
- Motivation
    1. Inherently distributed - banking
    2. Resource sharing
    3. Access geographically distributed resources
    4. Enhanced reliability - availability, integrity, fault-tolerance
    5. Increased performance/cost ratio
    6. Scalability
    7. Modularity and incremental expandability
- Flynn's taxonomy : Single/Multiple Instruction stream, Single/Multiple Data stream
- Coupling : degree of interdependency, binding, homogeneity
    * Strongly coupled - SIMD, MISD (common clocking/shared streams);
    * Weakly coupled - mulitcomputers over LAN, physically remote multicomputers
- Parallelism : measured as ratio of time $T(1)$ with single processor, to time
    $T(n)$ with $n$ processors, for a fixed program
- Concurrency : local operations / total operations
- Granularity : computation / communication
    * Coarse grained $\implies$ more computation / high granularity
    * Fine grained $\implies$ less computation / low granularity
- Multiprocessor/multicomputer operating systems
    * Network operating system : loosely coupled processors, loosely coupled
        software
    * Distributed operating system : loosely coupled processors, tightly coupled
        software (middleware)
    * Multiprocessor operating system : tightly coupled processors, tightly
        coupled software
- Shared memory : common address space throughout the system, communication via
    data variables and control variables (mutex/semaphore, monitors). Examples :
    UMA, NUMA etc
- Message passing : anything that is not shared memory

Note : each one can simulate the other

- MP $\rightarrow$ SM : partition address space per processor, simulate
    send/receive using write/read into these disjoint address spaces
- SM $\rightarrow$ MP : each shared memory as assigned to a process (its "owner"
  process). Read/write to a shared memory is querying/updating message to
  respective owner
- Buffered send : user buffer $\rightarrow$ kernel buffer $\rightarrow$ network
- Unbuffered send : user buffer $\rightarrow$ network
- Synchronous primitive : send/receive is synchronous if handshaking occurs -
    control returns back to send after acknowledgement from receive
- Asynchronous primitive : Control returns back to send right after data is
  copied out of buffer
- Blocking primitive : control returns to invoking process after processing for
    primitive
- Non-blocking primitive : control returns to invoking process right after
    invoking primitive
- Processor synchrony : all processors execute in lock-step with their clocks
    synchronized
- Drift rate : offset between processor clock and a very precise reference clock
    per unit time
- Asynchronous execution : no processor synchrony + unbounded drift rate,
    message delays finite but unbounded, no upper bound per step
- Synchronous execution : processors synchronized + bounded drift rate, message
    delays occur in one logical step/round, known upper bound on time take by a
    process to execute a step

## Challenges

### Systems

- Communication
- Processes
- Naming
- Synchronization
- Data storage and access
- Consistency and replication
- Fault tolerance
- Security
- API
- Scalability and modularity

### Algorithmic

- Physical clock synchronization
- Leader election
- Mutual exclusion
- Deadlock detection and resolution
- Termination detection
- Garbage collection
