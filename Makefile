PROJECT=columbo

ESCRIPT_BEAMS ?= "ebin/*", "deps/*/ebin/*"

include erlang.mk

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))

script:
	make escript
