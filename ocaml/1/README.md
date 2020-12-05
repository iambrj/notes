* If no `Base` or any other external libraries are being used, executable can be
    built using
    ```
    ocamlopt freq.ml -o freq
    ```
* With `Base` and `Stdio`
    ```
    ocamlfind ocamlopt -linkpkg -package base -package stdio freq.ml -o freq
    ```
* With `dune`, have a `dune` file named
    ```
    (executable
        (name      freq)
        (libraries base stdio))
    ```
    and do
    ```
    dune build freq.exe
    ```
