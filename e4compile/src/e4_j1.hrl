-ifndef(J1_HEADER).
-define(J1_HEADER, 1).

-define(J1BITS, 16).
-define(J1OP_CMD_WIDTH, 3).
-define(J1_LITERAL_BITS, (?J1BITS-1)). % how many bits remain after lit flag bit
-define(J1OP_INDEX_WIDTH, (?J1BITS-?J1OP_CMD_WIDTH)).
%% Bit values for the first nibble (Instruction type)
-define(J1LITERAL,          8). % top bit set for literals
-define(J1INSTR_JUMP,       0).
-define(J1INSTR_JUMP_COND,  1).
-define(J1INSTR_CALL,       2).
-define(J1INSTR_ALU,        3).
%% Bit values for the second nibble (Operation)
-define(J1OP_T,             0).
-define(J1OP_N,             1).
-define(J1OP_T_PLUS_N,      2).
-define(J1OP_T_AND_N,       3).
-define(J1OP_T_OR_N,        4).
-define(J1OP_T_XOR_N,       5).
-define(J1OP_INVERT_T,      6).
-define(J1OP_N_EQ_T,        7).
-define(J1OP_N_LESS_T,      8).
-define(J1OP_N_RSHIFT_T,    9).
-define(J1OP_T_MINUS_1,     10).
-define(J1OP_R,             11).
-define(J1OP_INDEX_T,       12).
-define(J1OP_N_LSHIFT_T,    13).
-define(J1OP_DEPTH,         14).
-define(J1OP_N_UNSIGNED_LESS_T, 15).

-record(alu, {
    op = 0 :: 0..15,    % one of ?J1OP_* macros.
    tn = 0 :: 0..1,     % copy T (stack top) -> N (next after stack top)
    rpc = 0 :: 0..1,    % copy R (return stack top) to PC (program counter)
    tr = 0 :: 0..1,     % copy T (stack top) to R (return stack top)
    nti = 0 :: 0..1,    % indexed RAM access N->[T]
    ds = 0 :: 0..3,     % 2 bits, signed increment of data stack
    rs = 0 :: 0..3      % 2 bits, signed increment of return stack
}).
-type alu() :: #alu{}.

-endif. % J1_HEADER
