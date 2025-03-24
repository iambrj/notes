# Chapter 2
- Re-entrant lock: same thread can acquire same lock any number of times (e.g.
  subclass first acquires it and calls superclass method which again acquires).
- Every java reference can be used as a lock via the `synchronized` block.

# Chapter 3
- The compiler can reorder memory management instructions causing *visibility*
  issues: changes made to shared memory by one thread are not visible to other
  threads.
- Visibility can happen due to objects being stored in cache etc.
- `volatile` keyword can be used to mark a reference as being used by multiple
  threads and disable caching, forcing the JVM to load the object each time.
- The most common use for volatile variables is as a completion, interruption,
  or status flag.
- Use volatile variables iff:
  1. Writes to variable does not depend on current value
  2. Variable does not participate in invariants with other state variables
  3. Locking is not required for any other reason while variable is being
     accessed
- Semantics of volatile variable are not strong enough to make increment
  operator ++ atomic.
- Out-of-thin-air safety: a shared variable read by a thread will have a value
  written to it by some other thread (and not some garbage value). Only
  exception is non-volatile 64-bit numbers (long/double).
- Publishing an object: making it available to code outside of its current
  scope.
- Publishing an object should be done in a thread-safe, synchronized manner.
  Otherwise an incompletely constructed object may get published.
- Any object reachable from a published object by following some chain of
  nonprivate field references and method calls has also been published.
- Alien method of a class C is any method whose behaviour is not fully
  specified by C. E.g. methods in other class instances of C are passed to and
  overrideable methods in C.
- (anonymous) inner classes also capture the reference to containing superclass
  and can publish the object.
- Publishing an object from within its constructor can publish an incompletely
  constructed object.

# Chapter 5
- Collections like `Map`, `List`, etc would require threads to lock entire
  collection to ensure correctness with concurrent access.
- `ConcurrentModificationException`: an unchecked exception thrown when a
  collection that is being read (via an iterator) is changed concurrently by
  some other thread.
- `ConcurrentModificationException` is a good faith exception meaning best
  effort is made to throw it, but no guarantee that it will thrown upon a
  concurrent modification.
- Hidden iterators like `println("Set = " + set)` can also throw
  `ConcurrentModificationException` since above code gets translated to a call
  to an iteration of the set.
- `ConcurrentHashMap`:
   + arbitrary number of readers
   + limited number of writers
   + low performance penality on single core
   + weakly consistent (approximately correct): iterator may or may not reflect
     concurrent updates
   + `size` and `isEmpty` are approximate
   + only when application needs to lock Map for exclusive access the
     ConcurrentHashMap is not correct replacement
- `ConcurrentHashMap` interface:
   + `putIfAbsent` (atomic check and insert)
- Copy-on-write collection
- `BlockingQueue`: blocks threads on queue underflow and overflow.
- `BlockingQueue` interface:
   + `add`
   + `offer`
   + `take`
   + `poll`
- Implementations of `BlockingQueue`:
   + LinkedBlockingQueue
   + ArrayBlockingQueue
   + PriorityBlockingQueue
   + SynchronousQueue: queue of threads, consumer should always be ready to
     participate in hand-off from producer; otherwise producer will block
- Work stealing pattern: consumers can consume from other consumer's queue
- Deque: double eneded queue used for work stealing pattern
- Synchronizer: object used to coordinate control flow of threads based on
  shared state of syncrhonizer.

- `Latch`: synchronizer used to delay progress of thread until latch reaches
  terminal state.
- `CountDownLatch` interface:
   + `await`
   + `countDown`

- `CyclicBarrier`: synchronizer that allows a set of threads to all wait for
  each other to reach a common barrier point repeatedly.
- `CyclicBarrier` interface:
   + `await`

- `Semaphore` used to control number of threads that can access a certain
  resource (e.g. file descriptor) or perform a given action at a time.
- `Semaphore` controls a set of virtual permits which threads can acquire and
  release.
- `Semaphore` interface:
   + `acquire`
   + `release`.
- `Semaphore` is fair if FIFO is followed in granting the permits.

# Chapter 6
- Disadvantages of unbounded thread creation:
  + Threadlife cycle overhead
  + Idle thread increase resource consumption (CPU, garbage collection, etc)
  + JVM has implementation limit on number of allowed threads, will throw
    `OutOfMemoryError` upon hitting this limit which is hard to recover from
- `Executor`: separates task submission from task execution.
- Tasks are wrapped in Runnable or Callable and submitted to an executor.
- Lifecycle of a task submitted to an executor:
  1. Created
  2. Submitted
  3. Started
  4. Completed
- ExecutionException: exception in which a tasks exception gets wrapped when a
  thread calls task's future.get
- RejectedExecutionException: exception thrown when a task submitted to an
  executor is not run.
- `Executor` interface:
   + `execute(Runnable)`

# Chapter 7
- Threads in blocked state throw `InterruptedException`, if a thread is not
  blocked state and gets interrupted, then the thread's interrupted status flag
  is set but no exception is thrown.
- Swallowing the `InterruptedException` is bad practise as swallowing makes it
  not possible for methods up the call stack to interrupt a thread.
- `Thread.interrupted()` fetch and clear the interruption status flag.
- `Thread.interrupt()` set the interruption status flag and cause an
  `InterruptedException` in invoked thread if blocked.
- Either a thread should propogate the `InterruptedException` or reset the
  interruption status using `Thread.currentThread().interrupted()`
- Tasks do not execute in threads they own; they borrow threads owned by a
  service such as a thread pool. Code that doesn’t own the thread (for a thread
  pool, any code outside of the thread pool implementation) should be careful
  to preserve the interrupted status so that the owning code can eventually act
  on it, even if the “guest” code acts on the interruption as well.
- You should know a thread’s interruption policy before interrupting it.

# Chapter 14
- Use `wait`, `notify`, `notifyAll` on any `Object` for conditional variables.
