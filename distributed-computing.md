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
    1. (C1) Messages are conserved : $send(m_{ij})\in LS_i\implies m_{ij}\in
       SC_{ij}\oplus rec(m_{ij})\in LS_j$
    2. (C2) Every effect has a cause : $send(m_{ij})\not\in LS_i\implies
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

    Time complexity - $O(network\ diameter)$

    Space complexity - $O(edge\ count)$
- Properties
    1. Recorded global state may not correspond to any global states that occur
       during computation, as a process can change its state asynchronously
       before the markers it sent are received by others and they record their
       states. However, by permuting events occurred during execution, it is
       possible to reach recorded global state from initial global state
    2. All recorded process states are mutually logically concurrent
    3. Rubberband criterion
- Lai-Yang (Non-FIFO) tag messages sent after snapshot (using color scheme for
    messages etc) and have all processes maintain message histories across
    channels to compute channel states
- Causal order : for any process, order in which it receives messages cannot
    violate happened-before relation of the corresponding sends
- For process state, a token is broadcast by an initiator process and all
    processes reply with their state when they receive the token. This gives
    process state recording, for channel state recording there are two ways
- Acharya-Badrinath channel state recording
    1. Each process, along with local state, replies with arrays $SENT_i[1\dots
       N]$ and $RECD_i[1\dots N]$
    2. State channel from $p_i$ to $p_j$ is set of messages with sequence
       numbers $\{RECD_j[i] + 1,\dots, SENT_i[j]\}$ and all state channels from
       initiator are empty
This requires $2n$ messages and $2$ time units for recording and assembling the
snapshot

- Alagar-Venkatesan :

# Mutual exclusion

- Computation model
    1. Messages aren't lost, channels don't fail
    2. Each site has one process
    3. Processes can make at most one request for CS
    4. States : requesting CS, executing CS, idle (neither)
- Requirements of mutual exclusion
    1. Saftey : at most one process can be executing CS
    2. Liveness/Progress : no infinite waits; requesting site should get CS in
       finite time
    3. Fairness : in order by logical time
- Complexity measures
    1. Message complexity : \# of messages required per cs execution by a site
    2. Synchronization Delay : time between leaving and next process entering CS
    3. Response time : time between request and exiting CS
    4. System throughput : 1 / (SD + E); rate of request execution
- Lamport's mutual exclusion algorithm
    1. $S_i$ broadcasts $REQUEST(ts_i, i)$ to all sites and places it on
       $request\_queue_i$ when it wants ot enter CS
    2. When $S_k$ receives $REQUEST(ts_i, i)$ it places it onto
       $request\_queue_k$ and it returns a timestamped $REPLY$ message to $S_i$

    $S_i$ enters CS when the following two conditions are met

    1. $S_i$ has received a message with timestamp larger than $ts_i$ from all
       other sites (need not be reply message)
    2. $S_i$'s request is a the top of $request\_queue_i$

    On releasing the CS

    1. $S_i$ removes request from top of queue and broadcasts $release_i$
       message to all sties
    2. When $S_k$ receives $release_i$ message, it removes $S_i$'s request from
       its request queue
Note that this does not work for nonFIFO channels
- Complexity
    1. Message complexity : without optimization, needs $3(N - 1)$ messages
       (request, reply and release each (N - 1))per cs invocation. After
       optimization, between $3(N - 1)$ and $2(N - 1)$
    2. Requires channels to be FIFO
    3. Synchronization delay : max message transmission time for release message
    4. Requests granted in order of time stamps
- Ricart-Agarwala
    1. All processes have reply deferred boolean arrays $RD_i[N]$
    2. When $P_i$ wants to enter CS, it broadcasts request to all sites.
    3. When $P_j$ receives request, it replies if
        1. It is neither requesting nor executing CS
        2. It is requesting, but its request started after $P_i$'s request
    4. After $P_i$ exits the CS, it performs all deferred replies
- Complexity
    1. (N - 1) request and (N - 1) reply = 2(N - 1) messages
    2. Does not require FIFO
    3. Need array space, no heap space
    4. Sync delay = max message transmission time
    5. Requests granted in order of increasing timestamps
- Roucairol-Carvalho : can reenter immediately if reply not sent yet. Message
    complexity between 0 and $2(N - 1)$
- Maekawa : instead of requesting from all processes, request from some subset
    1. Requirements for request set $R_i$ of each process
        - For all $i, j$, $R_i\cap R_j\neq\phi$
        - For all $i$, $i\in R_i$
        - For all $i$, $|R_i| = K$ for some $K$
        - Each node is contained in exactly $D$ request sets

        One possible value : $K = D = \sqrt{N}$
    2. Simple version
        1. For $P_i$ to enter CS, it broadcasts request to all processes in
           $R_i$
        2. On receiving request, send reply if no reply has been sent since
           previous release. Update taht reply has been sent. Otherwise add to
           queue.
        3. $P_i$ enters CS if it receives reply from all nodes in $R_i$
        4. When $P_i$ exits CS, it broadcasts release to all nodes in $R_i$.
        5. On receiving release, send reply to next node in queue and delete the
           node (how to select next is not specified).
        6. If queue is empty, update states to indicate no reply has been sent.

    Complexity

    1. Message complexity is Sublinear $O(\sqrt{N})$ ($3 *\sqrt{N}$)
    2. However, this may deadlock! To remove deadlock, need $5 * \sqrt{N}$
       messages by using extra failed, inquire and yield messages
    3. Sync delay = 2 * (max message transmission time)
- To avoid deadlocks
    1. Failed : if reply already given to someone else with higher priority
    2. Inquire : try to reset queue if higher priority request arrives after
       reply has been sent
    3. Yield : Process got failed form some process in the group so it gives up
       desire to enter cs.

    Complexity
    1. Same sync delay 2 * (max message transmission time)
    2. Message complexity $5 * \sqrt{N}$
    3. Deadlock free

- Suzuki Kasami
    1. Token has FIFO queue of requesting processes and $LN[1\dots n]$ and each
       process has $RN_i[1\dots n]$
    2. For $i$ to request CS, increment $RN_i[i]$ and send $REQUEST(i, RN_i[i])$
       to all nodes. If it already has token, enter if no pending requests.
    3. On receiving $REQUEST(i, sn)$, set $RN_j[i] = max(RN_j[i], sn)$ and send
       to $i$ if token is idle.
    4. On release, set $LN[i] = RN_i[i]$ and append to queue all those $j$ such
       that $RN_i[j] = LN[j] + 1$ (i.e. requests that arrived while $i$ was in
       CS). If queue is nonempty, delete first node and send token.
- Properties
    + No starvation
    + Message complexity : 0 (token with node) or $n$
    + Synchronization delay : 0 (token with node) or max message delay
- Raymond's algorithm : nodes arranged in tree with each node having FIFO queue
    1. If $i$ wants to enter, it places request on $Q_i$. If $i$ doesn't hold
       the token, it sends REQUEST to its parent
    2. If $j$ receives REQUEST, then it places it in $Q_j$ and sends REQUEST to
       its parent if no previous REQUEST has been sent
    3. When root receives REQUEST, send token to that node and set parent to
       that node
    4. When a node receives a token, delete the first queue entry, send token
       there, update parent and send REQUEST if queue is not empty (since it
       must be forwarded again)
    5. If token is received and node's own entry is at the top delete entry and
       enter CS
    6. On release, if queue is non-empty, delete first entry from the queue,
       send token to that node and make that the parent.
- Properties
    + Average message complexity $O(log\ n)$
    + Sync delay = $(T\ log\ n) / 2$ ($T$ = max message delay)
    + Can cause starvation
