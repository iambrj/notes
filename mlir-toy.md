# Chapter 1
- Dialect: grouping functionality to extend MLIR system by adding new operators,
  attributes, and types. A dialect consists of:
  + A prefix / namespace
  + A list of custom types, each its C++ class
  + A list of operations, each its name and C++ class implementation
    - Verifier for operator invariants (e.g. toy.print must have a single operand)
    - Semantics (no-side-effects, constant-folding, CSE-allowed)
  + Passes: analysis, transformations, dialect conversions
  + Possibly custom parser and assembly printer
- Operations are defined by
    + Name
    + List of SSA operand values
    + List of attributes (MLIR's mechanism for attaching constant metadata to
        operations)
    + List of types for result values
    + Source location
    + List of successor blocks
    + List of regions
- Region: list of basic blocks. Approximately CFG.
- Traits: mechanism to inject additional behavior (accessors, verification, etc)
  into operators.
- MLIR allows undefined operations to be manipulated through opaque `Operaton`
  class.

# Chapter 3: High-level transformations
- `RewritePattern`s are used to represent term rewrites.
- Canonicalization framework: used to reduce operations to canonical forms.

# Chapter 4: Generic transformations
- MLIR declares abstract classes for high level transformations which can be
  defined on operation specific derived classes.
- DialectInlinerInterface

# Chapter 5: Partial Lowering
- `DialectConversion` framework is used to convert operations from one dialect
  to another.
- To use the `DialectConversion` framework, we provide it with two things:
  1. A specification of what operations are legal and illegal, used to decide
     what to convert and what to not.
  2. A set of patterns used to convert illegal operations into a set of zero or
     more legal operations.
- `DialectConversion` framework also uses `RewritePattern`s, it uses a derived
  class called `ConversionPattern`.
## TODO
- Spelunk adaptor TransposeOpAdaptor
- Spelunk helper lowerOpToLoops

# Chapter 6: LLVM Lowering
- LLVMDialect
- TypeConverter
