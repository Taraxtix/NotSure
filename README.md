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
    - `exit [arg];` (Exit the program with `[arg]` as an exit code)
    - `let identifier = [arg];` (Declare the variable `identifier` and initialize it with `[arg]`)
    - `identifier = [arg];` (Changes the value of `identifier` to `[arg]`)
An argument (`[arg]`) can be one of the followed :
    - `integer_literal`
    - `( [arg] )`
    - `[arg] [bin_op] [arg]`
    - `identifier` (The value stored by `identifier`)
The binary operators (`[bin_op]`) that are currently supported are:
    - `*` (Multiplication)
    - `/` (Integer Division)
    - `%` (Modulo)
    - `|` (Bitwise OR)
    - `&` (Bitwise AND)
    - `+` (Addition)
    - `-` (Substraction)
