-ifndef(E4_CORE_ERLANG_HRL).
-define(E4_CORE_ERLANG_HRL, 1).

-include_lib("compiler/src/core_parse.hrl").

-type cerl_lhs() :: #c_literal{} | #c_var{} | #c_tuple{}. % TODO: binary, map
-type cerl_rhs() :: cerl_lhs().
-type cerl_ast_element() :: #c_literal{} | #c_alias{} | #c_apply{} | #c_binary{}
| #c_bitstr{} | #c_call{} | #c_case{} | #c_catch{} | #c_clause{}
| #c_cons{} | #c_fun{} | #c_let{} | #c_letrec{} | #c_map{} | #c_map_pair{}
| #c_module{} | #c_primop{} | #c_receive{} | #c_seq{} | #c_try{}
| #c_tuple{} | #c_values{} | #c_var{}.
-type cerl_ast() :: cerl_ast_element() | [cerl_ast_element()].
-type cerl_module() :: #c_module{}.

-endif. % E4_CORE_ERLANG_HRL
