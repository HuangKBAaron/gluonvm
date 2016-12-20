# Name the module which is used for test compilation
#TESTMODULE=gb_trees
TESTMODULE=test1

.PHONY: run
run:
	rebar3 compile && ./e4c.sh priv/$(TESTMODULE).erl

.PHONY: dialyze
dialyze:
	rebar3 dialyzer

.PHONY: test
test:
	rebar3 ct
