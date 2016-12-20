-ifndef(E4_KERNEL_ERLANG_HRL).
-define(E4_KERNEL_ERLANG_HRL, 1).

-include_lib("compiler/src/v3_kernel.hrl").

-type k_literal() :: #k_literal{}.
-type k_int() :: #k_int{}.
-type k_float() :: #k_float{}.
-type k_atom() :: #k_atom{}.
-type k_nil() :: #k_nil{}.
-type k_tuple() :: #k_tuple{}.
-type k_map() :: #k_map{}.
-type k_map_pair() :: #k_map_pair{}.
-type k_cons() :: #k_cons{}.
-type k_binary() :: #k_binary{}.
-type k_bin_seg() :: #k_bin_seg{}.
-type k_bin_int() :: #k_bin_int{}.
-type k_bin_end() :: #k_bin_end{}.
-type k_var() :: #k_var{}.
-type k_local() :: #k_local{}.
-type k_remote() :: #k_remote{}.
-type k_internal() :: #k_internal{}.
-type k_mdef() :: #k_mdef{}.
-type k_fdef() :: #k_fdef{}.
-type k_seq() :: #k_seq{}.
-type k_put() :: #k_put{}.
-type k_bif() :: #k_bif{}.
-type k_test() :: #k_test{}.
-type k_call() :: #k_call{}.
-type k_enter() :: #k_enter{}.
-type k_receive() :: #k_receive{}.
-type k_receive_accept() :: #k_receive_accept{}.
-type k_receive_next() :: #k_receive_next{}.
-type k_try() :: #k_try{}.
-type k_try_enter() :: #k_try_enter{}.
%%-type k_protected() :: #k_protected{}.
-type k_catch() :: #k_catch{}.
-type k_guard_match() :: #k_guard_match{}.
-type k_match() :: #k_match{}.
-type k_alt() :: #k_alt{}.
-type k_select() :: #k_select{}.
-type k_type_clause() :: #k_type_clause{}.
-type k_val_clause() :: #k_val_clause{}.
-type k_guard() :: #k_guard{}.
-type k_guard_clause() :: #k_guard_clause{}.
-type k_break() :: #k_break{}.
-type k_guard_break() :: #k_guard_break{}.
-type k_return() :: #k_return{}.

-type k_ast_element() :: #c_literal{} | #c_alias{} | #c_apply{} | #c_binary{}
    | #c_bitstr{} | #c_call{} | #c_case{} | #c_catch{} | #c_clause{}
    | #c_cons{} | #c_fun{} | #c_let{} | #c_letrec{} | #c_map{} | #c_map_pair{}
    | #c_module{} | #c_primop{} | #c_receive{} | #c_seq{} | #c_try{}
    | #c_tuple{} | #c_values{} | #c_var{}.
-type k_ast() :: k_ast_element() | [k_ast_element()].

-endif. % E4_KERNEL_ERLANG_HRL
