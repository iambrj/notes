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

# A model of distributed computations

- Each process performs its computations in a sequential manner
- Events are indivisible, finite, atomic
- Computations of each process are modeled as three types of events
    1. Internal event
    2. Message send event
    3. Message receive event
- Occurrence of events affects processes and global system state
    1. Internal events only affect corresponding process
    2. Message send events (may) affect both sending and receiving processes
    3. Message receive events (may) affect both sending and receiving processes
- Events at a particular process are linearly ordered
- Lamport's happens before relation $e_i\rightarrow e_j$ means event $e_i$
    happens before event $e_j$
- Denotes flow of information, availability of information
- Properties of $\rightarrow$
    1. $\rightarrow$ imposes a nonreflexive partial order
    2. $\forall e_i$ and $e_j$, $e_i\not\rightarrow e_j\not\implies
       e_j\not\rightarrow e_i$
    3. $\forall e_i$ and $e_j$, $e_i\rightarrow e_j\implies
       e_j\not\rightarrow e_i$
- Concurrent events : $(e_i\not\rightarrow e_j)\land (e_j\not\rightarrow
    e_i)\implies e_i\parallel e_j$, i.e. two events are *logically*
    concurrent iff they do not effect each other causally
- Note that $\parallel$ is **not** transitive
- Logical concurrency $\implies$ two events not causually affecting each other.
    Physical concurrent $\implies$ occur same instant in physical time.
- Models of communication networks : FIFO (queue) vs Non-FIFO (set) vs Causal
    Ordering (happens before for send and receive for messages)
- Global state = local states of processes + communication channels

# Logical time

- Logical clock maps events to time domain
- Consistency condition
    \begin{align*}
    e_i\rightarrow e_j\implies C(e_i) < C(e_j)
    \end{align*}
- Strongly consistent
    \begin{align*}
    e_i\rightarrow e_j\iff C(e_i) < C(e_j)
    \end{align*}
- All clocks with representation for tracking
    1. Logical local clock
    2. Logical global clock
- Protocols consist of
    1. R1 : how is local logical clock updated by process when it executes an
       event
    2. R2 : how global logical clock is updated

## Scalar time

- Single integer $C_i$ represents both local logical clock and global time
    1. R1 : Before each event (send/receive/internal), increment
    \begin{align*}
    C_i := C_i + d
    \end{align*}
    Note that $d$ may potentially be different during each instance.
    2. R2 : When process $p_i$ receives message with timestamp $C_{msg}$
        1. $C_i = max(C_i, C_{msg})$
        2. execute R1
        3. deliver message
- Properties of scalar time
    1. Consistent (but not strongly consistent)
    2. Total ordering over events
    3. When $d$ is fixed at 1, if event $e$ has timestamp $h$ then $h - 1$
       represents the minimum logical duration required before producing event
       $e$

## Vector time

- Each process maintains a vector of timestamps, where $vt_i[j]$ represents
    $P_i's$ record of logical time at $P_j$
    1. R1 : Before event, increment local logical time
    \begin{align*}
    C_i := C_i + d
    \end{align*}
    2. R2 : Each message piggybacked with $vt$ of sender
        1. $1\leq k\leq n : vt_i[k] = max(vt_i[k], vt[k])$
        2. execute R1
        3. deliver $m$
- Comparing vector clocks
    \begin{align*}
    vh = vk \iff\forall x : vh[x] = vk[x]\\
    vh \leq vk \iff\forall x : vh[x] \leq vk[x]\\
    vh < vk \iff vh\leq vk\land \exists x : vh[x] < vk[x]\\
    vh\parallel vk\iff \lnot(vh < vk)\land\lnot (vk < vh)
    \end{align*}
- Properties
    1. Isomorphism between set of partially ordered events and their vector
       timestamps
       \begin{align*}
       x\rightarrow y \iff vh < vk\\
       x\parallel y \iff vh \parallel vk
       \end{align*}
    2. Strong consistency
    3. Event counting
    4. Applications : distributed debugging, causal communication etc
- Singhal-Kshemkalyani's differential technique : instead of sending entire
    vectors, just send `{(position, new value)}` for changed timestamps

## Matrix time

- Each process maintains $n\times n$ matrix with following entries
    1. $m_i[i, i]$ = local logical clock of $p_i$
    2. $m_i[i, j]$ = $p_i$'s record of $p_j$'s local time
    3. $m_i[j, k]$ = $p_i$'s record of $p_j$'s record of $p_k$'s local time
- Update rules are as follows
    1. R1
    \begin{align*}
    m_i[i, i] := m_i[i, i] + d
    \end{align*}
    2. R2
        1. Update $mt_i[i,*]$ with received timestamp
        2. $mt_i[k,l] = max(mt_i[k, l], mt[k, l])$ for all $1\leq k,l\leq n$
        3. R1
        4. Deliver message

# Global state and snapshot recording algorithms

- Messages sent but not yet received are called transit messages. For channel
    $C_{ij}$
    \begin{align*}
    transit(LS_i, LS_j) = \{m_{ij} | send(m_{ij})\in LS_i\land
    rec(m_{ij})\not\in LS_j\}
    \end{align*}
    where $LS$ is local state of respective process. Therefore local state
    information gives channel information.
- FIFO channel preserves message order
- Global state of a distributed system is a collection of the local states of
    the processes and the channels
    \begin{align*}
    GS = \{\cup_i LS_i, \cup_{i, j} SC_{ij}\}
    \end{align*}
- A global state is consistent if
    1. Messages are conserved : $send(m_{ij})\in LS_i\implies m_{ij}\in
       SC_{ij}\oplus rec(m_{ij})\in LS_j$
    2. Every effect has a cause : $send(m_{ij})\not\in LS_i\implies
       m_{ij}\not\in SC_{ij}\land rec(m_{ij})\not\in LS_j$
- Cut : partitioning the spacetime diagram such that everything to the left of
    the partition is the past and everything to the right is the future. Since
    messages can only go from past to future, cuts with messages flowing the
    other way are called **inconsistent cuts**
- Two major issues
    1. Which messages to record and which to not?\
    From C1 : Any message sent by a process *before* taking its snapshot must be
    recorded.\
    From C2 : Any message sent by a process *after* taking its snapshot must *not* be
    recorded.
    2. What instance should a process take its snapshot?
    From C2 : $p_j$ must snapshot before processing $m_{ij}$ sent by $p_i$ after
    taking its snapshot
- Chandy-Lamport : send marker token along channels after snapshotting before
    any other messages are sent.
    1. Sending rule
        1. $p_i$ records its state
        2. Send markers on all outgoing channels before other messages
    2. Receiving rule : on receiving marker along $C$\
    if $p_j$ hasn't snapshotted then\
    record $C$'s message state as empty and execute sending rule\
    else\
    record $C$'s message state as those received between $p_j$'s snapshot and
    marker
