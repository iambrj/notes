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
- Avoid spinlock by yeilding
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
