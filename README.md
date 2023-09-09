# NotSure

A C-like programming language made for learning purpose

## Task

- [X] Compiled
- [ ] Statically typed
- [ ] Turing Complete
- [ ] Self Hosted

## Documentation

A program is a list of statements.

A statement can be one of the followed :

- `syscall [digit] [arg]` (Call a syscall with `[digit]` arguments not including the syscall code which go first)
- `{ [Statement]* }` (Create a scope within which every variable that are declared will be cleared at the end of it)
- `[Control Flow]`
- `dbg [arg]` (print the u64 value of `[arg]` followed by a new line)
- `exit [arg];` (Exit the program with `[arg]` as an exit code)
- `let identifier = [arg];` (Declare the variable `identifier` and initialize it with `[arg]`)
- `identifier = [arg];` (Changes the value of `identifier` to `[arg]`)

An argument (`[arg]`) can be one of the followed :

- `string_literal`
- `char_literal`
- `integer_literal`
- `( [arg] )`
- `[arg] [bin_op] [arg]`
- `identifier` (The value stored by `identifier`)
- `&identifier` (The address where `identifier` is stored)

The binary operators (`[bin_op]`) that are currently supported are:

- `*` (Multiplication)
- `/` (Integer Division)
- `%` (Modulo)
- `|` (Bitwise OR)
- `&` (Bitwise AND)
- `+` (Addition)
- `-` (Subtraction)
- `==` (Equal to)
- `>` (Greater than)
- `<` (Less than)
- `>=` (Greater or Equal than)
- `<=` (Less or Equal than)
- `&&` (Logical AND)
- `||` (Logical OR)

Control Flow block (`[ControlFlow]`) that are actually supported are:

- `if [arg] do [statement]`
- `if [arg] do [statement] else [statement]`
- `while [arg] do [statement]`
