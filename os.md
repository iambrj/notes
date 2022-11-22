# Memory

- Virtual memory: abstraction over RAM to create beautiful illusion

## Segmentation
- Break up process's address space into some number of variable-sized logical
  segments (code, stack, heap, etc)
- Segmentation: have per [code|stack|heap] segment base-and-bounds. MMU has to
  track base and bounds of each segment of each process separately.
- Base and bounds memory management: physical address = virtual address + base
- This is coarse grained segmentation, as it chops up memory in relatively
  large, coarse chunks.
- Free space management via free lists
- Internal vs external fragmentation
- Segregated lists: maintain separate list for frequently asked chunk sizes
- Binary buddy allocator: think of memory as a segment tree, and allocate
  segments that fit the requested size

## Paging
- Paging: allocate fixed-sized units/pages
- Physical memory = array of fixed-sized slots, called page frames
- Per-process data structure called page table, which stores address
  translations (virtual -> physical)
- Status bits: valid bit, protection bit, present bit, dirty bit, reference bit
- Translation-Lookaside Buffer: hardware cache for popular virtual->physical
  mappings
- How to update TLB cache misses? LRU, random, etc

# Concurrency

## Thread properties
- Light-weight process, shares open resources, code segment, address space, etc
  with other threads in group
- Registers and [kernel|user] call stack private per thread

## POSIX threads api
- `pthread_create`
- `pthread_join`
- `pthread_signal`
- `pthread_mutex_lock`
- `pthread_mutex_unlock`
- `pthread_mutex_t`
- Can use separate lock per data structure variable to have fine grained
  parallelism.

## Locks
- Race condition
```
1. mov 0x42, %rax
2. add1 %rax
3. mov %rax 0x42
```
- Solution : use locks
```
0. lock(l)
1. mov 0x42, %rax
2. add1 %rax
3. mov %rax 0x42
4. unlock(l)
```
- Implementation metrics:
  + Does it work correctly?
  + Is it fair? Or are threads starved?
  + Is it efficient?
- Disable interrupts 
- Test-and-set hardware primitive, spinlock
```
int test-and-set(int *ptr, int val) {
  int old = *ptr;
  *ptr = val;
  return old;
}

int l = 0;

void lock(int l) {
  while(test-and-set(l, 1)) yeild();
}

void unlock(int l) {
  l = 0;
}
```
- Other hardware primitives:
  + Compare-and-swap hardware primitive, spinlock
  + Load-linked and store-conditional
  + Fetch-and-add
- Avoid spinlock by yielding
- Linux futex : implement in-kernel queue per futex
- Lock with queue
```
typedef struct __lock_t {
  int l = 0;
  int l_q;
  queue_t q = {};
} lock_t;

void lock(int l, thread_t tid) {
  while(test-and-set(l_q, 1) == 1) ;
  if(q.empty() || q.front() == tid) {
    while(test-and-set(l, 1) == 1) {
      q.push(tid);
      l_q = 0;
      yield();
    }
    if(q.font() == tid) {
      q.pop();
    }
    l_q = 0;
  } else { // I am not in the front
    l_q = 0;
    yield();
  }
}
```

## Conditional Variables
- Conditional variable is a queue of threads waiting for a condition to become
  true
- `pthread_cond_t`
- Operations on conditional variable:
  + `pthread_cond_wait(c, m)` : when a thread wishes to put itself to sleep
  + `pthread_cond_signal(c)` : when a thread wants to wake a sleeping thread
- Always hold lock `m` when waiting (mandated by semantics) and signalling
  (simplest solution, though not always required)
- Bounded-buffer/Producer-consumer. Two pitfalls:
  1. Use separate conditional variables for waiting producers and waiting
     consumers
  2. Use `while` instead of `if`, some implementations may wakeup multiple
     threads on single signal
```

int box, count = 100, boxFull = 0;
cond_t c;
mutex_t m;

int consumer() {
  int sum = 0;
  for(int i = 1; i <= count; i++) {
    lock(m);
    while(boxFull == 0) wait(c, m);
    sum += box;
    boxFull = 0;
    signal(c);
    unlock(m);
  }
  return sum;
}

void producer() {
  for(int i = 1; i <= count; i++) {
    lock(m);
    while(boxFull == 1) wait(c, m);
    box = i;
    boxFull = 1;
    signal(c);
    unlock(m);
  }
}

int main() {
  pthread_create(producer);
  pthread_create(consumer);
  pthread_create(consumer);
}
```

## Semaphore
- General data structure that has the behavior of both mutex/lock and
  conditional variable
- API : `sem_wait` and `sem_post`
  ```
  int sem_wait(sem_t *s) {
    wait if value negative, decrement value of s by 1
  }
  int sem_post(sem_t *s) {
    increment value of s by 1, wake one waiting thread
  }
  ```
- Exercise: how can mutex be implemented using a semaphore?
  ```
  void lock(sem_t s) {
    sem_wait(s);
  }

  void unlock(sem_t s) {
    sem_post(s);
  }

  sem_t s = 1;

  thread1, thread2:
    lock(s)
    mov 0x42, %rax
    add1 %rax
    mov %rax 0x42
    unlock(s)
  ```
- Exercise: how to sequence events using semaphore?
  ```
  sem_t s = 0;
  thread2:
    printf("Child!");
    sem_post(s);
  thread1 : 
    printf("Parent!");
    sem_wait(s);
    pthread_create(thread2);

  // 100101
  sem_t s = 0;
  thread0:
    while(1) {
      printf("0")
      sem_post(s)
    }
  thread1:
    while(1) {
      printf("1")
      sem_wait(s)
    }
  main:
    pthread_create(thread0)
    pthread_create(thread1)
  ```
- Semaphore intuition: how many resources are you willing to give away
  immediately after initialization?
- Semaphore invariant: negative number indicates number of waiting threads
- Exercise: bounded buffer using semaphore?
  ```
  int box;
  sem_t s = 0;

  producer:
    for(i : 1 to 100) {
      box = i;
      sem_post(s);
    }

  consumer:
    for(i : 1 to 100) {
      sem_wait(s);
      printf("Got %d", box);
    }
  ```
- Exercise: reader-writer using semaphore?
- Exercise: dining philosophers using semaphore?
  + No deadlock
  + No starvation
  + High concurrency
- 
