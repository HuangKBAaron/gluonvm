-module(e4_f).

%% API
-export([
    'and'/1, 'if'/2, 'if'/3, block/0, block/1, block/3, block/4, comment/1,
    comment/2, equals/2, lit/1, match_two_values/2, nil/0, evaluate/1,
    store/1, tuple/1, var/1, element/2, unless/2, mark_alias/2,
    mark_new_var/1, mark_new_arg/1, make_mfarity/3, primop/2, include/1
]).

-include_lib("compiler/src/core_parse.hrl").
-include("e4_forth.hrl").

%% Takes list of Forth checks and creates forth instructions which produce
%% true if all conditions are true. Assumption: each Cond in Conds is a Forth
%% sequence which leaves one value on stack
'and'([]) -> [];
'and'(Conds) ->
    %% Remove true clauses
    Conds1 = lists:filter(
        fun(#c_literal{val='true'}) -> false;
            ([]) -> false;
            (_) -> true
        end,
        Conds),
    case Conds1 of
        [] -> [];
        _ -> [Conds1, lists:duplicate(length(Conds1) - 1, 'AND')]
    end.

%% TODO separate Cond from the rest of the arguments. How to optimize if true?
'if'(#f_lit{val='true'}, Body = #f_block{}) ->
    Body;
'if'(Cond, Body = #f_block{}) ->
    block(
        [comment("begin if"), Cond, <<"IF">>],
        [Body],
        [<<"THEN">>]
    ).

'if'(Cond, Body = #f_block{}, Else = #f_block{}) ->
    block(
        [comment("begin ifelse"), Cond, <<"IF">>],
        [Body, <<"ELSE">>, Else],
        [<<"THEN">>]
    ).

unless(#f_lit{val='false'}, _Block) -> [];
unless(Cond, Body = #f_block{}) ->
    block(
        [comment("begin unless"), Cond, <<"UNLESS">>],
        [Body],
        [<<"THEN">>]
    ).

%% ( c b a N - {a,b,c} , constructs a tuple size N from values on stack )
%% Takes list of Forth expressions where each leaves one value on stack
%% and constructs a tuple of that size
tuple(Values) ->
    [
        lists:reverse(lists:map(fun evaluate/1, Values)),
        lit(length(Values)),
        <<".MAKE-TUPLE">>
    ].

%% ( X Y -- (X==Y) , takes 2 values from stack, pushes comparison result )
equals(Lhs, Rhs) -> [Lhs, Rhs, <<"==">>].

%% ( X Y -- X , if X==Y, otherwise badmatch error )
match_two_values(L, R) ->
    [   % TODO: move to core.fs
        evaluate(L), <<"DUP">>, evaluate(R), <<"==">>,
        <<"UNLESS">>, <<"ERROR-BADMATCH">>, <<"THEN">>
    ].

%% ( -- Value , leaves a literal value on stack )
lit(Value) -> #f_lit{val=Value}.

comment(Str) -> comment("~s", [Str]).

comment(Format, Args) ->
    Txt = iolist_to_binary(io_lib:format(Format, Args)),
    #f_comment{comment=Txt}.

%% ( -- nil , leaves value [] on stack )
nil() -> <<"NIL">>.

block() -> block([], [], [], []).
block(Code) -> block([], Code, [], []).

-spec block(Before :: intermediate_forth_code(),
            Code :: intermediate_forth_code(),
            After :: intermediate_forth_code()) -> f_block().
block(Before, Code, After) ->
    block(Before, Code, After, []).

-spec block(Before :: intermediate_forth_code(),
            Code :: intermediate_forth_code(),
            After :: intermediate_forth_code(), [f_var()]) ->
    f_block().
block(Before, Code, After, Scope) ->
    #f_block{before=Before, code=Code, 'after'=After, scope=Scope}.

%% ( X -- , stores value X on stack into variable Dst )
store(Dst = #f_var{}) -> #f_st{var=Dst}.

%% @doc If both args are variables, creates an alias for the next compiler
%% pass and generates no code. Otherwise generates code for copying.
mark_alias(Var = #f_var{}, Existing = #f_var{}) ->
    #f_var_alias{var=Var, existing=Existing};
mark_alias(Existing = #f_var{}, #f_stacktop{}) ->
    [<<"DUP">>, store(Existing)].

%% ( -- X , retrieves value of variable V or inserts an expression result )
%% ASSUMPTION: that expression will not grow the stack
evaluate(#c_call{module=M, name=N, args=Args}) ->
    #f_call{mod=M, fn=N, args=lists:map(fun evaluate/1, Args)};
evaluate(#c_tuple{es=Es}) -> tuple(Es);
evaluate(#c_apply{op=FunObj, args=Args}) ->
    #f_apply{
        funobj=evaluate(FunObj),
        args=lists:map(fun evaluate/1, Args)
    };
evaluate(#f_apply{} = A) -> A;
evaluate(I) when is_integer(I) -> lit(I);
evaluate(#f_stacktop{}) -> [];
evaluate(Retr = #f_ld{}) -> Retr;
evaluate(Lit = #f_lit{}) -> Lit;
evaluate(#c_literal{val=Lit}) -> lit(Lit);
evaluate(#c_var{name={F, A}}) when is_atom(F), is_integer(A) ->
    #f_mfa{mod='.', fn=F, arity=A};
evaluate(#c_var{name=Var}) -> #f_ld{var=Var};
evaluate(Var = #f_var{}) -> #f_ld{var=Var};
evaluate(OtherCode) ->
    block([comment("begin eval")],
        OtherCode,
        [comment("end eval")]).

var(#c_var{name=Name}) -> #f_var{name=Name};
var(#f_var{}=CF) -> CF;
var(Name) when is_atom(Name) -> #f_var{name=Name}.

mark_new_var(#c_var{}=V) -> #f_decl_var{var=var(V)};
mark_new_var(#f_var{}=V) -> #f_decl_var{var=var(V)}.
%%mark_new_var(Name) -> #f_decl_var{var=var(Name)}.

mark_new_arg(#f_var{}=V) -> #f_decl_arg{var=var(V)}.

element(Index, Tuple) ->
    [evaluate(Tuple), evaluate(Index), <<".GET-ELEMENT">>].

make_mfarity(M, F, Arity) when is_atom(F), is_atom(F) ->
    #f_mfa{mod=M, fn=F, arity=Arity};
make_mfarity(MExpr, FExpr, Arity) ->
    [MExpr, FExpr, lit(Arity), <<".MAKE-MFARITY">>].

primop(#c_literal{val=Primop}, Arity) -> primop(Primop, Arity);
primop(match_fail, 1) ->
    <<"ERROR-CASE-CLAUSE">>;
primop(Name, Arity) ->
    e4:compile_error("E4: Unknown primop ~p/~p", [Name, Arity]).

include(F) -> #f_include{filename=F}.
