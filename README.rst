Gluon VM
========

Virtual machine written in C++ to run custom flavour of J1 16-bit Forth.
Your program is compiled from Core Erlang to J1 Forth.

J1 Forth
--------

Includes a cross-compiler ``e4compiler``, which converts your Erlang source
to Core Erlang and then produces a Forth program (in memory) which is then
compiled into J1 Forth 16-bit bytecode and written to disk.

J1 Forth bytecode is (TODO) extended to accomodate multiple Erlang term types
and longer arguments (currently 16 bit instruction, 13 bit offsets, 15 bit
literals).

See Also
--------

``e4compiler/README.rst`` and ``e4compiler/docs``

The project is intended to land on embedded devices or something like RTEMS
(to be decided) but development naturally happens on x64 Linux.
