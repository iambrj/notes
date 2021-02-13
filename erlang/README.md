- To start the repl, enter in the shell
    ```
    $ erl
    ```
- Load module as follows
    ```
    1> c(factorial).
    ```
where `factorial.erl` has
    ```
    -module(factorial).    
    -export([factorial/1]).
    factorial(0) -> 1;                   
    factorial(N) -> N * factorial(N - 1).
    ```
- To evaluate an expression
    ```
    6> factorial:factorial(5).
    120
    ```
- Syntax for function export `function/argc`
- Only `export`ed functions can be run from outside the module
- Use `=` to perform pattern matching
    ```
    17> N = {banana, 12}.
    {banana,12}
    18> {A, B} = N.
    {banana,12}
    ```
- Use `_` for don't care patterns
    ```
    19> {C, _} = N.
    {banana,12}
    ```
- `spawn/3` : create a process
- `spawn(Module, Function, ArgList) -> pid()` : causes `Function` to be executed in
    parallel with the calling process
- `receive` : receives a message from a process
    ```
    receive
        Message1 ->
            ...;
        Message2 ->
            ...;
            ...
    end
    ```
- `send` : sends a message to a process
    ```
    Pid ! {a, 12}
    ```
- Terms (data types) provided by erlang
    1. Constant data types
        1. Numbers : integers and floats
        2. Atoms : begin with lowercase letter and terminated by
           non-alphanumeric character
        3. Pids
        4. References
    2. Compound data types
        1. Tuples `{a, 12, 'hello'}`
        2. Lists `[1, abc, [12], 'foo bar']`
- Strings are lists of characters
- Pattern matching for lists `[1, 2, 3 | Foo]`
- `appy(Module, Function, ArgList)`
- For inter module calls, `-import(Module, FunctionList)`
- `-` before `module`, `import`, `export` etc is called attribute prefix
- Clause ::= head -> body
- `case` syntax
    ```
    case Expr of
        Pattern1 [when Guard1] -> Seq1;
        Pattern2 [when Guard2] -> Seq2;
        ...
        PatternN [when GuardN] -> SeqN;
    end
    ```
- `if` syntax
    ```
    if
        Guard1 ->
            Sequence1;
        Guard2 ->
            Sequence2;
        ...
    end
    ```
- `receive` iterates through the mailbox messages, NOT receive clauses
- To receive message from specific process use `{Pid, Msg}` pattern
- use `after` for setting up time outs
    ```
    receive
        Message1 [when Guard1] ->
            ...;
        Message2 [when Guard1] ->
            ...;
            ...
    after
        TimeOutExpr -> % milliseconds
            ...
    end
    ```
- Applications of `after`
    + `sleep`
        ```
        sleep(Time) ->
        receive
            after Time ->
            true
        end.
        ```
    + `flush_buffer()`
        ```
        flush_buffer() ->
            receive
                _ ->
                    flush_buffer()
                after 0 ->
                    true
            end.
        ```
        works since no matter what the timeout value is, all messages currently
        in mailbox get tried
    + Priority receives, i.e. bypassing mailbox list
        ```
        priority_receive() ->
            receive
                urgent_message ->
                    urgent_action
            after 0 ->
                receive
                    normal_message ->
                        normal_action
                end
            end
        ```
- Manipulating names
    1. `register(Name, Pid)`
    2. `unregister(Name)`
    3. `whereis(Name)`
    4. `registered()`
- Use `process_flag(priority, Pri)` (where Pri is either `low` or `normal`) to
    change priority
- `group_leader()` returns the group leader for the evaluating process
- `groupleader(Leader, Pid)` sets the group leader of process `Pid` to the
    process `Leader`
