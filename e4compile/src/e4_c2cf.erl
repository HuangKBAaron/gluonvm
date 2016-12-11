%%% @doc From Core Erlang produces intermediate Core Forth syntax tree with
%% scopes and variable accesses marked
-module(e4_c2cf).

%% API
-export([process/1, process_code/2, module_new/0, get_code/1,
         emit/2]).

-include_lib("compiler/src/core_parse.hrl").
-include("e4.hrl").

-type cerl_lhs() :: #c_literal{} | #c_var{} | #c_tuple{}. % TODO: binary, map
-type cerl_rhs() :: cerl_lhs().
-type cerl_ast_element() :: #c_literal{} | #c_alias{} | #c_apply{} | #c_binary{}
    | #c_bitstr{} | #c_call{} | #c_case{} | #c_catch{} | #c_clause{}
    | #c_cons{} | #c_fun{} | #c_let{} | #c_letrec{} | #c_map{} | #c_map_pair{}
    | #c_module{} | #c_primop{} | #c_receive{} | #c_seq{} | #c_try{} | #c_tuple{}
    | #c_values{} | #c_var{}.
-type cerl_ast() :: cerl_ast_element() | [cerl_ast_element()].

module_new() -> #cf_mod{}.

-spec process(#c_module{}) -> cf_block().
process(#c_module{name=_Name, exports=_Exps, defs=Defs}) ->
    %M0 = #e4module{module=Name#c_literal.val},
    Block = e4_cf:block(
        [e4_cf:comment("begin mod")],
        [],
        [e4_cf:comment("end mod")]),
    Out = process_fun_defs(Block, Defs),
    io:format("~s~n", [format_code(Out)]).

add_code(Block = #cf_block{code=C}, AddCode) ->
    Block#cf_block{code=[AddCode| C]}.

-spec process_fun_defs(cf_block(), cerl_ast()) -> cf_block().
process_fun_defs(ModB, []) -> ModB;
process_fun_defs(ModB0, [{#c_var{name={Name, Arity}}, #c_fun{} = Fun} | Remaining]) ->
    Block1 = e4_cf:block(
            [':', format_fun_name(Name, Arity)],
            [compile_fun(Fun)],
            [';', e4_cf:comment("end fun ~s/~p", [Name, Arity])]),
    ModB1 = add_code(ModB0, Block1),
    process_fun_defs(ModB1, Remaining).

compile_fun(#c_fun{vars=Vars, body=Body}) ->
    %% Assume stack now only has reversed args
    ReverseArgs = lists:reverse(lists:map(fun e4_cf:var/1, Vars)),
    Block0 = e4_cf:block([], [], [], ReverseArgs),
    process_code(Block0, Body).

-spec process_code(cf_block(), cerl_ast()) -> cf_block().
process_code(Block, []) -> Block;
process_code(Block0, [CoreOp | Tail]) ->
    Block1 = process_code(Block0, CoreOp),
    process_code(Block1, Tail);

process_code(Block0, #c_case{arg=Arg, clauses=Clauses}) ->
    %% Arg = Tree, Clauses = [Tree]
    lists:foldl(
        fun(Clause, Blk) ->
            pattern_match(Blk, Arg, Clause)
        end, Block0, Clauses);

process_code(Block0, #c_literal{val=Value}) ->
    emit(Block0, e4_cf:lit(Value));

process_code(Block0, #c_let{vars=Vars, arg=Arg, body=Body}) ->
    ReverseVars = lists:map(fun e4_cf:var/1, Vars),
    LetBlock = e4_cf:block(
        [e4_cf:comment("begin let")],
        [],
        [e4_cf:comment("end let")],
        Block0#cf_block.scope ++ ReverseVars),

    LetBlock1 = process_code(LetBlock, Arg),
    process_code(LetBlock1, Body);

process_code(Block0, #c_apply{op=Op, args=Args}) ->
    emit(Block0,
         lists:reverse(lists:map(fun e4_cf:retrieve/1, Args)) ++ [
             length(Args), e4_cf:retrieve(Op), 'APPLY'
        ]
    );

process_code(Block0, #c_call{module=M, name=N, args=Args}) ->
    emit(Block0,
         lists:reverse(lists:map(fun e4_cf:retrieve/1, Args)) ++ [
            [e4_cf:retrieve(M),
             e4_cf:retrieve(N),
             length(Args)]
        ]);

process_code(Block0, #c_primop{name=Name, args=Args}) ->
    emit(Block0, ['?primop', f_val(Name), Args]);

process_code(Block0, #c_tuple{es=Es}) ->
    emit(Block0, e4_cf:tuple(lists:map(fun e4_cf:retrieve/1, Es)));

process_code(Block0, #c_cons{hd=H, tl=T}) ->
    emit(Block0, ['?cons', H, T]);

process_code(Block0, #c_var{name=N}) ->
    emit(Block0, ['?var', N]);

process_code(Block0, #c_alias{var=Var, pat=Pat}) ->
    emit(Block0, ['?alias', Var, Pat]);

process_code(_Block, X) ->
    compile_error("Unknown Core AST piece ~p~n", [X]).

%%make_var(#c_var{name=N}) -> #cf_var{name=N}.

%% TODO: this plays no specific role, redo this or rename
f_val(#c_literal{val=Unwrap}) -> e4_cf:lit(Unwrap);
f_val(#c_var{name={Fun, Arity}}) -> #cf_funarity{fn=Fun, arity=Arity};
f_val(#c_var{name=N}) -> e4_cf:var(N);
f_val(#c_tuple{es=Es}) -> e4_cf:tuple(Es);
f_val({_, nil}) -> e4_cf:nil();
f_val({_, Value}) -> #cf_lit{val=Value};
f_val(X) -> X. % assume nothing left to unwrap

%% Takes list [code and lazy elements] which are callable fun/1
%% (fun(State) -> emit... end) and runs the lazy elements combining
%% the output together
-spec emit(Block :: cf_block(), Code :: cf_op() | cf_code()) -> cf_block().
emit(Block, AddCode) when not is_list(AddCode) ->
    emit(Block, [AddCode]);
emit(Block, AddCode) ->
    lists:foldl(
        fun(Nested, Blk) when is_list(Nested) ->
                emit(Blk, Nested);
%%            (NewBlk = #cf_block{code=NewCode}, Blk = #cf_block{code=Code}) ->
%%                NewBlk1 = NewBlk#cf_block{code=lists:reverse(NewCode)},
%%                Blk#cf_block{code=[NewBlk1 | Code]};
            (ForthOp, Blk = #cf_block{code=Code}) ->
                %% TODO: Fix me i'm slow
                Blk#cf_block{code=Code++[ForthOp]}
                %% Blk#cf_block{code=[ForthOp | Code]}
        end,
        Block,
        AddCode).

format_fun_name(Name, Arity) ->
    #cf_mfarity{mod='.', fn=Name, arity=Arity}.

%%format_fun_name(Mod, Name, Arity) when is_atom(Mod) ->
%%    #cf_mfarity{mod=Mod, fn=Name, arity=Arity}.

%%f_fun_ref(#e4module{module=Mod}, #e4lit{val=Name}, Arity) ->
%%    format_fun_name(Mod, Name, Arity);
%%f_fun_ref(#e4lit{val=Mod}, #e4lit{val=Name}, Arity) ->
%%    #e4mfa{mod=Mod, fn=Name, arity=Arity}.

%% Builds code to match Arg vs Pats with Guard
%% Pats = [Tree], Guard = Tree, Body = Tree
pattern_match(State, Args, #c_clause{pats=Pats, guard=Guard, body=Body}) ->
    pattern_match_2(State, Pats, Args, Guard, Body).

%% For each element in Arg match element in Pats, additionally emit the
%% code to check Guard
-spec pattern_match_2(cf_block(),
                      Pats :: [cerl_ast()], Args0 :: cerl_ast(),
                      Guard :: cerl_ast(), Body :: cerl_ast()) -> cf_block().
pattern_match_2(Block0, Pats, Args0, _Guard, _Body) ->
    %% Convert to list if c_values is supplied
    Args1 = case Args0 of
                #c_values{es=Es} -> Es;
                _ -> Args0
            end,
    %% Convert to list if it was a single tuple
    Args = case is_list(Args1) of
               true -> Args1;
               false -> [Args1]
           end,
    %% Pair args and pats and compare
    PatsArgs = lists:zip(Pats, Args),
    lists:foldl(
        fun({Pat, Arg}, Blk) -> pattern_match_pairs(Blk, Pat, Arg) end,
        Block0, PatsArgs).

var_exists(#cf_block{scope=Scope}, #cf_var{} = Var) ->
    lists:member(Var, Scope).

%% @doc Given state and left/right side of the match, checks if left-hand
%% variable existed: if so - emits comparison, else introduces a new variable
%% and emits the assignment.
-spec pattern_match_pairs(cf_block(),
                          Lhs :: cerl_lhs() | cf_var(),
                          Rhs :: cerl_rhs() | cf_var())
                         -> cf_block().
pattern_match_pairs(Block0, Lhs, #c_var{name=RhsName}) ->
    pattern_match_pairs(Block0, Lhs, e4_cf:var(RhsName));
pattern_match_pairs(Block0 = #cf_block{scope=Scope0}, #c_var{name=LhsName}, Rhs) ->
    Lhs = #cf_var{name=LhsName},
    case var_exists(Block0, Lhs) of % if have variable in scope
        true -> % variable exists, so read it and compare
            emit(Block0, e4_cf:match_2_known(e4_cf:retrieve(Lhs), Rhs));
        false -> % introduce variable and use it
            Block1 = Block0#cf_block{scope = [Lhs | Scope0]},
            pattern_match_var_versus(Block1, Lhs, Rhs)
    end;
pattern_match_pairs(Block0, #c_literal{val=LhsLit}, Rhs) ->
    emit(Block0, e4_cf:match_2_known(
        e4_cf:lit(LhsLit),
        e4_cf:retrieve(Rhs)
    ));
pattern_match_pairs(Block0 = #cf_block{}, #c_tuple{es=LhsElements}, Rhs) ->
    pattern_match_tuple_versus(Block0, LhsElements, Rhs);
pattern_match_pairs(_State, Lhs, Rhs) ->
    compile_error("Match ~9999p versus ~9999p not implemented", [Lhs, Rhs]).

-spec pattern_match_var_versus(Block :: cf_block(),
                               cf_var(), cerl_rhs()|cf_var())
                              -> cf_block().
pattern_match_var_versus(Block0, #c_var{name=LhsName}, Rhs) -> % unwrap left
    Lhs = #cf_var{name=LhsName},
    pattern_match_var_versus(Block0, Lhs, Rhs);
pattern_match_var_versus(Block0, Lhs, #c_var{name=RhsName}) -> % unwrap right
    Rhs = #cf_var{name=RhsName},
    pattern_match_var_versus(Block0, Lhs, Rhs);
pattern_match_var_versus(Block0, #cf_var{} = Lhs, Rhs) ->
    case var_exists(Block0, Lhs) of
        true -> % var exists, so compare
            emit(Block0, [
                e4_cf:equals(e4_cf:retrieve(Rhs),
                             e4_cf:retrieve(Lhs)),
                e4_cf:comment("compare-match ~p = ~p", [Lhs, Rhs])
            ]);
        false -> % var did not exist, so copy-assign
            emit(Block0, [
                e4_cf:retrieve(Rhs), e4_cf:store(Lhs),
                e4_cf:comment("assign-match ~p = ~p", [Lhs, Rhs])]
            )
    end;
pattern_match_var_versus(_Blk, L, R) ->
    compile_error("Match var ~9999p against ~9999p is not implemented", [L, R]).

pattern_match_tuple_versus(Block0, LhsElements, #c_var{}=Rhs) ->
    pattern_match_tuple_versus(Block0, LhsElements, e4_cf:var(Rhs));
pattern_match_tuple_versus(Block0, LhsElements, #cf_var{} = Rhs) ->
    %% Iterate a list of [{1,Lhs1}, {2,Lhs2}, ...] and get element from Rhs
    Pairs = lists:zip(lists:seq(1, length(LhsElements)),
                      LhsElements),
    %% check that Rhs is a tuple
    Block1 = emit(Block0, [
        e4_cf:'if'(
            [e4_cf:retrieve(Rhs), e4_cf:lit(length(LhsElements)), 'IS-TUPLE'],
            e4_cf:block()
        )
    ]),
    lists:foldl(
        fun({Index, Lhs1}, Blk0) ->
            Blk1 = emit(Blk0, e4_cf:element(Index, Rhs)),
            pattern_match_var_versus(Blk1, Lhs1, #cf_stack_top{})
        end,
        Block1, Pairs);
pattern_match_tuple_versus(_State, _Lhs, Rhs) ->
    compile_error("Match tuple vs ~9999p is not implemented", [Rhs]).

compile_error(Format, Args) ->
    E = lists:flatten(io_lib:format(Format, Args)),
    erlang:error(E).

get_code(#cf_mod{code=Code}) -> Code.

format_code(L) when is_list(L) ->
    [format_code(Item) || Item <- L];
format_code(#cf_block{before=B, scope=_S, code=C, 'after'=A}) ->
    [color:cyanb("\\ begin\n"),
     format_code(B),
     format_code(C),
     format_code(A),
     color:cyanb("\\ end\n")];
format_code(W) when is_atom(W) ->
    io_lib:format("~s ", [color:yellowb(str(W))]);
format_code(#cf_lit{val=L}) ->
    io_lib:format("'~s ", [color:magenta(str(L))]);
format_code(#cf_retrieve{var=#cf_var{name=V}}) ->
    io_lib:format("~s(~s) ", [color:green("LD"), color:greenb(str(V))]);
format_code(#cf_store{var=#cf_var{name=V}}) ->
    io_lib:format("~s(~s) ", [color:red("ST"), color:redb(str(V))]);
format_code(#cf_comment{comment=C}) ->
    io_lib:format("~s ~s~n", [color:cyan("\\"), color:cyan(C)]);
format_code(#cf_var{name=V}) ->
    io_lib:format("~s ", [color:blueb(str(V))]);
format_code(C) -> io_lib:format("~p ", [C]).

str(X) when is_atom(X) -> atom_to_list(X);
str(X) when is_binary(X) -> io_lib:format("~s", [X]);
str(X) -> lists:flatten(io_lib:format("~p", [X])).