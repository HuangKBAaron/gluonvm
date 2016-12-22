%%% @doc Pass 1: From Core Erlang produces intermediate Forth syntax tree with
%% scopes defined and constructions marked
-module(e4_pass1).

%% API
-export([process/1, process_code/2, module_new/0, get_code/1,
    emit/2, format_core_forth/2]).

-include_lib("compiler/src/core_parse.hrl").

-include("e4_kernel_erl.hrl").
-include("e4_forth.hrl").
-include("e4.hrl").

module_new() -> #f_mod_pass1{}.

-spec process(k_mdef()) -> f_block().
process(#k_mdef{name=Name, exports=_Exps, attributes=_Attr, body=Body}) ->
    Block0 = e4_f:block(
        [
            e4_f:comment("begin mod"),
            e4_f:include("forth-lib/e4core.fs"),
            <<"(">>, <<":MODULE">>, atom_to_binary(Name, utf8), <<")">>
        ],
        [],
        [e4_f:comment("end mod")]),
    Out = process_code(Block0, Body),
%%    Out = process_fun_defs(Block, Body),
    io:format("PASS1~n~p~n", [Out]),
%%    io:format("PASS1~n~s~n", [format_core_forth(Out, 0)]),
    Out.

-spec process_code(f_block(), k_ast()) -> f_block().
process_code(Block, []) -> Block;
process_code(Block0, [CoreOp | Tail]) ->
    Block1 = process_code(Block0, CoreOp),
    process_code(Block1, Tail);

process_code(ParentBlock,
             #k_fdef{func=Name, arity=Arity, vars=Vars, body=Body}) ->
    io:format("k_fdef name=~s vars~p~n", [Name, Vars]),
    Block1 = e4_f:block(
        [<<":">>, format_fun_name(Name, Arity)],
        [],
        [<<";">>, e4_f:comment("end fun ~s/~p", [Name, Arity])]),
    Block2 = lists:foldl(
        fun(A, Blk) -> emit(Blk, e4_f:mark_new_arg(A)) end,
        Block1, Vars
    ),
    Block3 = process_code(Block2, Body),
    emit(ParentBlock, Block3);

%%process_code(Block0, #k_var{name=Var}) ->
%%    io:format("k_var ~p~n", [Var]),
%%    emit(Block0, e4_f:eval(Var));

process_code(Block0, KM = #k_match{}) -> process_match_block(Block0, '_', KM);

process_code(Block0, #k_seq{arg=Arg, body=Body}) ->
    io:format("k_seq arg=~p~n  body=~p~n", [Arg, Body]),
    Block1 = process_code(Block0, Arg),
    process_code(Block1, Body);

process_code(Block0, #k_return{args=Args}) ->
    io:format("k_return args=~p~n", [Args]),
    Block1 = eval_args(Block0, Args),
    emit(Block1, [<<"RET">>]);

process_code(Block0, #k_enter{op=Op, args=Args}) ->
    io:format("k_enter op=~p args=~p~n", [Op, Args]),
    Block1 = eval_args(Block0, Args),
    emit(Block1, [Op, <<"JUMP">>]);

process_code(Block0, #k_call{op=Op, args=Args, ret=Ret}) ->
    io:format("k_call op=~p~n  args=~p~n  ret=~p~n", [Op, Args, Ret]),
    Block1 = eval_args(Block0, Args),
    Block2 = emit(Block1, [Op, <<"CALL">>]),
    emit(Block2, e4_f:store(Ret));

process_code(Block0, #k_put{arg=Arg, ret=Ret}) ->
    io:format("k_put arg=~p ret=~p~n", [Arg, Ret]),
    emit(Block0, [e4_f:eval(Arg), e4_f:store(Ret)]);

process_code(_Block, X) ->
    ?COMPILE_ERROR("E4Cerl: Unknown Core AST piece ~s~n",
                   [?COLOR_TERM(red, X)]).

%%%
%%% Compiling nested match + alt + select + type_/val_clause code structure
%%%

-record(match_ctx, {vars = [] :: [k_var()]}).
-type match_ctx() :: #match_ctx{}.
-type match_elem_group() :: k_match() | k_alt() | k_select() | k_type_clause()
    | k_val_clause() | k_seq() | list().

-spec process_match_block(f_block(), match_ctx() | '_', match_elem_group())
                         -> f_block().
process_match_block(Block0, '_', #k_match{vars=Vars, body=Body, ret=Ret}) ->
    Match0 = e4_f:block(
        [e4_f:comment("begin match")],
        [],
        [e4_f:comment("end match")]),
    Match1 = process_match_block(Match0, Vars, Body),
    Match2 = emit(Match1, [e4_f:store(Ret)]),
    emit(Block0, Match2);

process_match_block(Match0, Context, L) when is_list(L) ->
    lists:foldl(fun(El, Match) -> process_match_block(Match, Context, El) end,
        Match0, L);

process_match_block(Block0, Context, #k_alt{first=First, then=Then}) ->
%%    io:format("k_alt first=~p~nthen=~p~n", [First, Then]),
    Alt0 = e4_f:block(
        [e4_f:comment("begin alt")],
        [],
        [e4_f:comment("end alt")]),
    Alt1 = process_match_block(Alt0, Context, First),
    Alt2 = process_match_block(Alt1, Context, Then),
    emit(Block0, Alt2);

process_match_block(Block0, Context, #k_select{var=Var, types=Types}) ->
    io:format("k_select var=~p~ntypes=~p~n", [Var, Types]),
    Select0 = e4_f:block(
        [e4_f:comment("begin select")],
        [],
        [e4_f:comment("end select")]),
    Select1 = process_match_block(Select0, Context, Types),
    emit(Block0, Select1);

process_match_block(Block0, Context, #k_type_clause{type=Type, values=Values}) ->
    io:format("k_type_clause t=~p~n  val=~p~n", [Type, Values]),
    Type0 = e4_f:block(
        [e4_f:comment("begin type clause")],
        [],
        [e4_f:comment("end type clause")]),
    %% TODO: Check Context var type
%%    Type1 = lists:foldl(
%%        fun(V, Block) ->
%%            emit(Block, [e4_f:eval(V), <<".IS-TUPLE">>])
%%        end,
%%        Context#match_ctx.vars),
    Type2 = process_match_block(Type1, Context, Values),
    emit(Block0, Type1);

process_match_block(Block0, _Context, #k_val_clause{val=Val, body=Body}) ->
    io:format("k_val_clause val=~p~n  body=~p~n", [Val, Body]),
    Val0 = e4_f:block(
        [e4_f:comment("begin val clause")],
        [],
        [e4_f:comment("end val clause")]),
    %% TODO: introduce free variables
    %% TODO: add comparisons for bound variables
    Val1 = process_code(Val0, Body),
    emit(Block0, Val1);

process_match_block(Block0, _Context, #k_seq{}=Seq) ->
    process_code(Block0, Seq);

process_match_block(_Block0, Context, Other) ->
    ?COMPILE_ERROR("E4 Pass1: Match block unknown element ~s (context ~s)",
        [?COLOR_TERM(red, Other), ?COLOR_TERM(yellow, Context)]).

%% @doc Transform list of variables, literals and expressions into something
%% which evaluates them, or reads variable values or something. And leaves all
%% of them on the stack in the good order for a function call following after.
eval_args(Block, Args) ->
    lists:foldl(
        fun(A, Blk) -> emit(Blk, e4_f:eval(A)) end,
        Block, Args
    ).

-spec emit(Block :: f_block(), Code :: intermediate_forth_code()) -> f_block().
emit(Block, AddCode) when not is_list(AddCode) ->
    emit(Block, [AddCode]);
emit(Block, AddCode) ->
    lists:foldl(
        fun(Nested, Blk) when is_list(Nested) ->
                emit(Blk, Nested);
            (ForthOp, Blk = #f_block{code=Code}) ->
                Blk#f_block{code=Code ++ [ForthOp]}
        end,
        Block,
        AddCode).

format_fun_name(Name, Arity) ->
    #f_mfa{mod='.', fn=Name, arity=Arity}.

get_code(#f_mod_pass1{code=Code}) -> Code.

i(I) -> lists:duplicate((I-1) * 4, 32).

format_core_forth(L, Indent) when is_list(L) ->
    [format_core_forth(Item, Indent) || Item <- L];
format_core_forth(#f_block{before=B, scope=_S, code=C, 'after'=A}, Indent) ->
    [format_core_forth(B, Indent+1),
     format_core_forth(C, Indent+1),
     format_core_forth(A, Indent+1)];
format_core_forth(C, Indent) ->
    io_lib:format("~s~s~n", [i(Indent), format_op(C)]).

format_op(#f_apply{funobj=FO, args=Args}) ->
    io_lib:format("~s(~s;~s)", [color:whiteb("apply"), format_op(FO),
                                [format_op(A) || A <- Args]]);
format_op(#k_var{name=V}) -> color:blueb(str(V));
format_op(W) when is_atom(W) ->
    io_lib:format("~s", [color:whiteb(str(W))]);
format_op(W) when ?IS_FORTH_WORD(W) ->
    io_lib:format("~s", [color:whiteb(str(W))]);
format_op(#k_literal{val=L}) ->
    io_lib:format("'~s", [color:magenta(str(L))]);
format_op(#f_ld{var=V}) ->
    io_lib:format("~s(~s)", [color:green("retrieve"), format_op(V)]);
format_op(#f_st{var=#k_var{name=V}}) ->
    io_lib:format("~s(~s)", [color:red("store"), format_op(V)]);
format_op(#f_decl_var{var=#k_var{name=V}}) ->
    io_lib:format("~s(~s)", [color:blackb("var"), format_op(V)]);
format_op(#f_decl_arg{var=#k_var{name=V}}) ->
    io_lib:format("~s(~s)", [color:blackb("arg"), format_op(V)]);
format_op(#f_var_alias{var=V, existing=Alt}) ->
    io_lib:format("~s(~s=~s)", [
        color:blackb("alias"), format_op(V), format_op(Alt)]);
format_op(#f_comment{comment=C}) ->
    io_lib:format("~s ~s",
                  [color:blackb("\\"), color:blackb(C)]);
format_op(#f_mfa{mod=M, fn=F, arity=A}) ->
    io_lib:format("~s~s,~s,~s~s",
                  [
                      color:magentab("MFA("),
                      format_op(M),
                      format_op(F),
                      str(A),
                      color:magentab(")")
                  ]);
format_op(#f_include{filename=F}) ->
    io_lib:format("~s(~s)", [color:whiteb("include"), F]);
format_op(#k_var{} = Var) ->
    io_lib:format("~s", [format_op(Var)]).

str(X) when is_atom(X) -> atom_to_list(X);
str(X) when is_binary(X) -> io_lib:format("~s", [X]);
str({A, B}) when is_atom(A), is_integer(B) ->
    io_lib:format("~s/~p", [A, B]);
str(X) -> lists:flatten(io_lib:format("~p", [X])).
