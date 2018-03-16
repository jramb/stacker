# stacker

A Clojure library designed to implement a stack based REPL and execution engine.

## Usage

### Simple repl
Run `lein run` to start a quite simple repl.

Run with parameters (one or many) to start repl and process the commands from the parameter.

Example:

    lein run "4 5 * p q"
    => prints "20"
    
or

    lein run "4 5 *" p 355 113. "/" p q
    
The "q" is necessary to end the repl / the programm. If you leave that out, the
repl continues.

### Your own programs
You can extend the environment with your own commands and thus give your own
tools a convenient REPL:

1) extend the @default-env with your own functions (probably)
2) start with a (probably) empty stack, a simple empty list `()`
3) Repeat until tired:
   Apply tokens on the [stack env] pair. You can use `string-to-tokens`
   to convert a string to a list of tokens.
4) Alternatively you can call the provided `repl` on the start-stack and env,
   which starts an interactive repl for you.


## License

Copyright © 2017 Jörg Ramb

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
