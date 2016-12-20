%%% @doc Pattern Match to Decision Tree compiler
%%% See: http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf

-module(e4_pm).

%% API
-export([]).

-include_lib("compiler/src/core_parse.hrl").
-include("e4_forth.hrl").

%% @doc Given Core Erlang pattern match input, produces a decision tree
%% to solve the match expression.
-spec pattern_match(f_block(), cerl_lhs(), cerl_rhs()) -> f_block().
pattern_match(Block0 = #f_block{}, Left, Right) ->
    Block0.