%%% @doc Pattern Match to Decision Tree compiler
%%% See: http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf

-module(e4_pm).

%% API
-export([pattern_match_compile/3]).

-include("e4_core_erl.hrl").
-include("e4_forth.hrl").

%% @doc Given Core Erlang pattern match input.
%% Takes: a value (right) and list of clauses (left)
%% Returns: a decision tree to solve the match expression for all clauses
-spec pattern_match_compile(f_block(),
                            cerl_rhs(),
                            [cerl:c_clause()]) -> f_block().
pattern_match_compile(Block0 = #f_block{}, Arg, Clauses) ->
    Block0.
