# NotSure

A C-like programming language made for learning purpose

## Task

- [X] Compiled
- [X] Turing Complete (See `examples/rule110.ns`)
- [ ] Statically typed
- [ ] Self Hosted

## Documentation

A program is a list of statements.

A statement can be one of the followed :

- `{ [Statement]* }` (Create a scope within which every variable that are declared will be cleared at the end of it)
- `[Control Flow]`
- `dbg [arg]` (print the u64 value of `[arg]` followed by a new line)
- `exit [arg];` (Exit the program with `[arg]` as an exit code)
- `let identifier = [arg];` (Declare the variable `identifier` and initialize it with `[arg]`)
- `identifier = [arg];` (Changes the value of `identifier` to `[arg]`)
- `*[size]:([arg]) = [arg];` (Change the value at the address)

An argument (`[arg]`) can be one of the followed :

- `string_literal`
- `char_literal`
- `integer_literal`
- `( [arg] )`
- `identifier` (The value stored by `identifier`)
- `&:identifier` (The address where `identifier` is stored)
- `*[size]:[arg]` (The value stored at the address `[arg]`)
- `syscall [arg]` (Call the syscall with code being it's first argument followed by their usual arguments)
- `[arg] [bin_op] [arg]` (Depends on the case: See the `[bin_op]` section)

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
