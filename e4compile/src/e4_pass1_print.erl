%%% @doc Print the nice indented colored output of pass1
%%% @end

-module(e4_pass1_print).

-include("e4_forth.hrl").
-include("e4.hrl").

%% API
-export([format_core_forth/2]).

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
format_op(W) when is_atom(W) ->
    io_lib:format("~s", [color:whiteb(str(W))]);
format_op(W) when ?IS_FORTH_WORD(W) ->
    io_lib:format("~s", [color:whiteb(str(W))]);
format_op(#k_nil{}) ->
    io_lib:format("lit:[]", []);
format_op(#k_literal{val=L}) ->
    io_lib:format("lit:~s", [color:magenta(str(L))]);
format_op(#k_atom{val=A}) ->
    io_lib:format("atom:~s", [color:white(str(A))]);
format_op(#f_ld{var=V}) ->
    io_lib:format("~s(~s)", [color:green("retrieve"), format_op(V)]);
format_op(#f_st{var=#k_var{name=V}}) ->
    io_lib:format("~s(~s)", [color:red("store"), format_op(V)]);
format_op(#f_decl_var{var=#k_var{name=V}}) ->
    io_lib:format("~s(~s)", [color:blackb("decl-var"), format_op(V)]);
format_op(#f_decl_arg{var=#k_var{name=V}}) ->
    io_lib:format("~s(~s)", [color:blackb("decl-arg"), format_op(V)]);
format_op(#f_var_alias{var=V, existing=Alt}) ->
    io_lib:format("~s(~s=~s)", [
        color:blackb("alias"), format_op(V), format_op(Alt)]);
format_op(#f_comment{comment=C}) ->
    io_lib:format("~s ~s",
                  [color:blackb("\\"), color:blackb(C)]);
format_op(#k_local{name=N, arity=A}) ->
    io_lib:format("funarity:~s/~p", [color:whiteb(str(N)), A]);
format_op(#k_remote{mod=M, name=F, arity=A}) ->
    io_lib:format("mfarity:~s,~s,~s", [format_op(M), format_op(F), str(A)]);
format_op(#f_include{filename=F}) ->
    io_lib:format("~s(~s)", [color:whiteb("include"), F]);
%%format_op(#k_var{} = Var) ->
%%    io_lib:format("~s", [format_op(Var)]).
format_op(Other) ->
    ?COMPILE_ERROR("format_op: unknown ~p", [Other]).

str(X) when is_atom(X) -> atom_to_list(X);
str(X) when is_binary(X) -> io_lib:format("~s", [X]);
str({A, B}) when is_atom(A), is_integer(B) ->
    io_lib:format("~s/~p", [A, B]);
str(X) -> lists:flatten(io_lib:format("~p", [X])).

i(I) -> lists:duplicate((I-1) * 4, 32).
