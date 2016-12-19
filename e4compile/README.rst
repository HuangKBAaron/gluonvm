BEAM to Erl-Forth (E4) compiler
===============================

A simple cross-compiler, converts your Erlang source to Core Erlang and then
produces a Forth program which is then compiled into J1 Forth bytecode.

In default J1 implementation instructions are 16 bit each, address offsets for
jumps and calls are 13 bit and literals are 15 bit in width.
Multiple modifications are planned to extend this for running Erlang code,
such as tagging literal types, marking longer arguments (literal and address
continuations) etc.

Build
-----

    $ make run
    $ make dialyzer
    $ elvis rock
    $ make test

