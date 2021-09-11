- An anonymous module signature is created by `sig D1;...;Dn end`
  where each `Di` is either a type declaration or a value declaration. A module
  signature can be named using `module type SignatureName = sig D1;...;Dn end`.
- An anonymous module is created by `struct D1;...;Dn end` where each `Di` is a
  definition. A module can be named using `module ModuleName = struct
  D1;...;Dn end`.
- A module can be annotated by a signature iff it defines every declaration
    in the signature (it can however have extra definitions, which cannot be
    accessed from outside the module).
