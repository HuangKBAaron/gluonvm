%%% @doc Value helpers

-module(e4_helper).

%% API
-export([can_be_calculated/2]).

-include("e4_forth.hrl").
-include("e4.hrl").

%% @doc Consult with scope and find out if value contains only known parts
%% such as bound variables and literals, or some dynamic parts such as calls
%% and (free) unbound variables. Having a known or dynamic value allows
%% binding it to a temporary before some other calculation.
-spec can_be_calculated(f_block(), any()) -> boolean().
can_be_calculated(_Block, #k_literal{}) -> true;
can_be_calculated(_Block, #k_int{})     -> true;
can_be_calculated(_Block, #k_float{})   -> true;
can_be_calculated(_Block, #k_atom{})    -> true;
can_be_calculated(Block = #f_block{}, #k_tuple{es=Elements}) ->
    lists:all(fun(E) -> can_be_calculated(Block, E) end,
              Elements);
can_be_calculated(#f_block{scope=Scope}, #k_var{} = Var) ->
    scope_has(Scope, Var).

%% @doc Given a scope from an #f_block{} and a #k_var{} checks if the var exists
%% in that scope (bound) or doesn't (free)
scope_has(Scope, #k_var{name=Name}) ->
    lists:member(Name, Scope).
