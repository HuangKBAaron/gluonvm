%%% @doc Pass 1: From Core Erlang produces intermediate Forth syntax tree with
%% scopes defined and constructions marked
-module(e4_pass1).

%% API
-export([process/1, module_new/0, get_code/1, emit/2]).

-include_lib("compiler/src/core_parse.hrl").

-include("e4_kernel_erl.hrl").
-include("e4_forth.hrl").
-include("e4.hrl").

-record(match_ctx, {
    match_vars= [] :: [k_var()],    % vars which appeared in #k_match{}
    select_var,                     % focused var which appeared #k_select{}
    type :: k_tuple | k_atom | k_int
}).
-type match_ctx() :: #match_ctx{}.
%%-type match_elem_group() :: k_match() | k_alt() | k_select() | k_type_clause()
%%                          | k_val_clause() | k_seq() | list().

module_new() -> #f_mod_pass1{}.

-spec process(k_mdef()) -> f_block().
process(#k_mdef{name=Name, exports=_Exps, attributes=_Attr, body=Body}) ->
    Block0 = e4_f:block(
        [
            e4_f:comment("begin mod"),
            e4_f:include("forth-lib/e4core.fs"),
            e4_f:comment("MODULE ~s", [Name])
        ],
        [],
        [e4_f:comment("end mod")]),
    Out = process_code(Block0, #match_ctx{}, Body),
    io:format("PASS1~n~s~n", [e4_pass1_print:format_core_forth(Out, 0)]),
    Out.

p(L) -> e4_pass1_print:format_core_forth(L, 0).

-spec process_code(f_block(), match_ctx(), k_ast()) -> f_block().
process_code(Block = #f_block{}, #match_ctx{}, []) -> Block;

%% Special case returning a var if it comes last in the list
process_code(Block = #f_block{}, #match_ctx{}, #k_var{}=Var) ->
    emit(Block, e4_f:eval(Var));

%% Special case 2, a var is not allowed otherwise
%%process_code(Block = #f_block{}, #match_ctx{}, #k_var{}=Var) ->
%%    ?COMPILE_ERROR("A free-floating var is not allowed in the input: ~s",
%%        [?COLOR_TERM(red, Var)]);

process_code(Block0 = #f_block{}, Ctx = #match_ctx{}, [CoreOp | Tail]) ->
    Block1 = process_code(Block0, Ctx, CoreOp),
    process_code(Block1, Ctx, Tail);

process_code(Block0 = #f_block{}, Ctx = #match_ctx{},
             #k_fdef{func=Name, arity=Arity, vars=Vars, body=Body}) ->
%%    io:format("k_fdef name=~s vars~p~n", [Name, Vars]),
    Block1 = e4_f:block(
        [<<":">>, format_fun_name(Name, Arity)],
        [],
        [<<";">>, e4_f:comment("end fun ~s/~p", [Name, Arity])]),
    Block2 = lists:foldl(
        fun(A, Blk) -> emit(Blk, e4_f:mark_new_arg(A)) end,
        Block1, Vars
    ),
    Block3 = process_code(Block2, Ctx, Body),
    emit(Block0, Block3);

%%process_code(Block0, #k_var{name=Var}) ->
%%    io:format("k_var ~p~n", [Var]),
%%    emit(Block0, e4_f:eval(Var));

%%
%% Begin guards
%%
process_code(Block0 = #f_block{}, Ctx = #match_ctx{},
             #k_guard{clauses=Clauses}) ->
    lists:foldl(
        fun(C, Blk) ->
            process_code(Blk, Ctx, C)
        end, Block0, Clauses);
process_code(Block0 = #f_block{}, Ctx = #match_ctx{},
             #k_guard_clause{guard=G, body=Body}) ->
    ConditionCode = process_code(e4_f:block(), Ctx, G),
    BodyCode = process_code(e4_f:block(), Ctx, Body),
    emit(Block0, e4_f:'if'(ConditionCode, BodyCode));
%%
%% end guards

process_code(Block0 = #f_block{}, Ctx = #match_ctx{}, #k_try{} = Try) ->
    io:format("k_try~n", []),
    %% TODO k_try
    Try0 = e4_f:block(
        [e4_f:comment("begin try")],
        [],
        [e4_f:comment("end try")]),
    Try1 = process_code(Try0, Ctx, Try#k_try.arg),
    Try2 = process_code(Try1, Ctx, Try#k_try.body),
    emit(Block0, Try2);

process_code(Block0 = #f_block{}, Ctx = #match_ctx{},
             #k_seq{arg=Arg, body=Body}) ->
%%    io:format("k_seq arg=~p~n  body=~p~n", [Arg, Body]),
    Block1 = process_code(Block0, Ctx, Arg),
    process_code(Block1, Ctx, Body);

process_code(Block0 = #f_block{}, #match_ctx{}, #k_return{args=Args}) ->
%%    io:format("k_return args=~p~n", [Args]),
    Block1 = eval_args(Block0, Args),
    emit(Block1, [<<"RET">>]);

process_code(Block0 = #f_block{}, #match_ctx{}, #k_enter{op=Op, args=Args}) ->
%%    io:format("k_enter op=~p args=~p~n", [Op, Args]),
    Block1 = eval_args(Block0, Args),
    emit(Block1, [Op, <<"JUMP">>]);

process_code(Block0 = #f_block{}, #match_ctx{},
             #k_call{op=Op, args=Args, ret=Ret}) ->
%%    io:format("k_call op=~p~n  args=~p~n  ret=~p~n", [Op, Args, Ret]),
    Block1 = eval_args(Block0, Args),
    Block2 = emit(Block1, [e4_f:eval(Op), <<"CALL">>]),
    emit(Block2, e4_f:store(Ret));

process_code(Block0 = #f_block{}, #match_ctx{},
             #k_bif{op=Op, args=Args, ret=Ret}) ->
%%    io:format("k_bif op=~p~n  args=~p~n  ret=~p~n", [Op, Args, Ret]),
    Block1 = eval_args(Block0, Args),
    Block2 = emit(Block1, [e4_f:eval(Op), <<".BIF">>]),
    emit(Block2, e4_f:store(Ret));

process_code(Block0 = #f_block{}, #match_ctx{},
             #k_test{op=Op, args=Args}) ->
    %% Emit args
    Block1 = eval_args(Block0, Args),
    Block2 = emit(Block1, [e4_f:eval(Op), <<"CALL">>]),
%%    case Inv of
%%        true -> emit(Block2, <<".INVERT">>);
%%        false -> Block2
%%    end;
    Block2;
process_code(Block0, #match_ctx{}, #k_put{arg=Arg, ret=Ret}) ->
%%    io:format("k_put arg=~p ret=~p~n", [Arg, Ret]),
    emit(Block0, [e4_f:eval(Arg), e4_f:store(Ret)]);

process_code(Block0 = #f_block{}, Ctx = #match_ctx{},
             #k_alt{first=First, then=Then}) ->
    io:format("k_alt first=~p~nthen=~p~n", [First, Then]),
    Alt0 = e4_f:block(
        [e4_f:comment("begin alt")],
        [],
        [e4_f:comment("end alt")]),
    Alt1 = process_code(Alt0, Ctx, First),
%%    p(Alt1),
    io:format("Alt1 ~p~n", [Alt1]),
    Alt2 = process_code(Alt1, Ctx, Then),
    emit(Block0, Alt2);

process_code(Block0, #match_ctx{}, #k_match{vars=Vars, body=Body, ret=Ret}) ->
%%    io:format("k_match vars=~p~n", [Vars]),
    Match0 = e4_f:block(
        [e4_f:comment("begin match")],
        [],
        [e4_f:comment("end match")]),
    Match1 = process_code(Match0, #match_ctx{match_vars=Vars}, Body),
    Match2 = emit(Match1, [e4_f:store(Ret)]),
    emit(Block0, Match2);

process_code(Block0 = #f_block{}, Context0 = #match_ctx{},
             #k_select{var=Var, types=Types}) ->
%%    io:format("k_select var=~p~ntypes=~p~n", [Var, Types]),
    Select0 = e4_f:block(
        [e4_f:comment("begin select")],
        [],
        [e4_f:comment("end select")]),
    %% Update focused select var
    Context1 = Context0#match_ctx{select_var=Var},
    %% Go deeper
    Select1 = process_code(Select0, Context1, Types),
    emit(Block0, Select1);

process_code(Block0 = #f_block{}, Context = #match_ctx{},
            #k_type_clause{type=Type, values=Values}) ->
%%    io:format("k_type_clause t=~p~n  val=~p~n", [Type, Values]),
    Type0 = match_if_type(Type, Context),
    Context1 = Context#match_ctx{type = Type},
    Type1 = process_code(Type0, Context1, Values),
    emit(Block0, Type1);

process_code(Block0 = #f_block{},
             #match_ctx{select_var=Rhs, type=Type} = Context,
             #k_val_clause{val=Lhs, body=Body}) ->
%%    io:format("k_val_clause lhs=~999p~n"
%%              "  rhs=~p~n  body=~p~n", [Lhs, Rhs, Body]),
    Val00 = e4_f:block(
        [e4_f:comment("begin val clause")],
        [],
        [e4_f:comment("end val clause")]),
    %% In case RVal is a complex expression, save into tmp variable and emit
    %% the accompanying code (possibly empty list)
    {LTmp, LTmpEmit} = e4_f:make_tmp(Block0, Lhs),
    Val0 = emit(Val00, LTmpEmit),

    %% Create a conditional block for pattern match which checks if all
    %% values on the left match all values on the right.
    MatchBlock = emit_match(Block0#f_block.scope, Type, LTmp, Rhs),

    %% Process body of the clause
    Val1 = process_code(Val0, Context, Body),

    %% Insert processed code into match condition block
    MatchBlock1 = emit(MatchBlock, Val1),

    %% Now insert match condition block into the parent block
    emit(Block0, MatchBlock1);

process_code(#f_block{}, #match_ctx{}, X) ->
    ?COMPILE_ERROR("E4Cerl: Unknown Core AST piece ~s~n",
                   [?COLOR_TERM(red, X)]).

%% @doc Transform list of variables, literals and expressions into something
%% which evaluates them, or reads variable values or something. And leaves all
%% of them on the stack in the good order for a function call following after.
eval_args(Block, Args) ->
    lists:foldl(
        fun(A, Blk) -> emit(Blk, e4_f:eval(A)) end,
        Block, Args
    ).

-spec emit(Block :: f_block(), Code :: intermediate_forth_code()) -> f_block().
emit(Block = #f_block{}, AddCode) when not is_list(AddCode) ->
    emit(Block, [AddCode]);
emit(Block = #f_block{}, AddCode) ->
    lists:foldl(fun emit_x_into_y/2, Block, AddCode).

emit_x_into_y(#k_var{}, _Blk) ->
    ?COMPILE_ERROR("should not emit variable");
emit_x_into_y(Nested, Blk) when is_list(Nested) ->
    emit(Blk, Nested);
emit_x_into_y(ForthOp, Blk = #f_block{code=Code}) ->
    Blk#f_block{code=Code ++ [ForthOp]}.

format_fun_name(Name, Arity) ->
    #k_local{name=Name, arity=Arity}.

get_code(#f_mod_pass1{code=Code}) -> Code.

%% @doc Create an IF block which checks if Context variable(s) are a Type
match_if_type(k_literal, _Context) ->
    e4_f:block(); % do nothing just proceed to value clause and compare directly
match_if_type(Type, Context = #match_ctx{}) ->
    e4_f:'if'(
        [e4_f:eval(Context#match_ctx.select_var),
         make_type_check(Type)],
        e4_f:block()
    ).

make_type_check(k_nil) -> <<".NIL?">>;
make_type_check(k_atom) -> <<".ATOM?">>;
make_type_check(k_tuple) -> <<".TUPLE?">>.

%% @doc Create a conditional block for pattern match which checks if all
%% values on the left match all values on the right.
%% Code should be inserted inside this block by the caller.
emit_match(Scope, k_tuple, #k_tuple{es=LhsElements}, Rhs) ->
    %% Assuming Rhs is also a tuple, take elements from it and match against
    %% each of the LhsElements. Create new variables as needed.
    RhsElements = lists:map(
        fun(I) ->
            %% TODO: Push Rhs and dup for each get-element or something?
            %% Naive approach is to retrieve it every time
            [e4_f:eval(Rhs), e4_f:lit(I), <<".GET-ELEMENT">>]
        end,
        lists:seq(1, length(LhsElements))
    ),
    VarPairs = lists:zip(LhsElements, RhsElements),

    %% Now partition a list of [{L,R}, ... ] pairs into such, where L is
    %% known and can be compared and other, where L is an unbound name and
    %% will always match, producing a bound variable.
    PartitionFun = fun({L, _R}) -> e4_helper:scope_has(Scope, L) end,
    {ComparePairs, AssignPairs} = lists:partition(PartitionFun, VarPairs),

    %% Dump out the comparison-matching code
    Block0 = lists:foldl(
        fun({Lhs1, Rhs1}, Block0) ->
            emit(Block0, e4_f:equals(Lhs1, Rhs1))
        end,
        e4_f:block(),
        ComparePairs
    ),
    %% Dump out the assigning code
    lists:foldl(
        fun({Lhs1, Rhs1}, Blk) ->
            emit(Blk, [e4_f:eval(Rhs1), e4_f:store(Lhs1)])
        end,
        Block0,
        AssignPairs
    );
emit_match(_Scope, _, #k_var{} = L, #k_var{} = R) ->
    e4_f:block(e4_f:equals(L, R));
emit_match(_Scope, k_atom, [LhsVar], Rhs) ->
    e4_f:block(e4_f:equals(LhsVar, Rhs)).
