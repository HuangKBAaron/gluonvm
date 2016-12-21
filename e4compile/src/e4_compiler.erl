-module(e4_compiler).
-export([process/1]).

-include("e4_forth.hrl").

%% @doc Takes filename as input, produces compiled BEAM AST and processes it
process(F) ->
    case compile:file(F, [to_kernel, binary, report]) of
        {ok, M, Kernel} ->
            IR1 = e4:try_do("Pass1 - Kernel Erlang pass",
                            fun() -> e4_pass1:process(Kernel) end),
            IR2 = e4:try_do("Pass2 - Variables pass",
                            fun() -> e4_pass2:process(IR1) end),
            RealForth1  = e4:try_do("Pass3 - Convert to Forth",
                                    fun() -> e4_pass3:process(IR2) end),
            RealForth2  = e4:try_do("Pass4 - Opt",
                                    fun() -> e4_pass_opt1:process(RealForth1)
                                    end),
            J1Prog      = e4:try_do("Pass5 - Compile to J1 opcodes",
                                    fun() -> e4_j1c:compile(M, RealForth2) end),
            e4:try_do("Save binary output",
                fun() ->
                    IOList = e4_file:to_iolist(J1Prog),
                    file:write_file(e4_file:bin_filename(F),
                            iolist_to_binary(IOList))
                end);
        E ->
            io:format("~n~s: ~p~n", [F, E])
    end.
