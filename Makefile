REBAR = $(shell pwd)/rebar3

.PHONY: deps rel stagedevrel package version all

all: cp-hooks compile

cp-hooks:
	cp hooks/* .git/hooks

version:
	@echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > dalmatiner_frontend.version

version_header: version
	@echo "-define(VERSION, <<\"$(shell cat dalmatiner_frontend.version)\">>)." > apps/dalmatiner_frontend/include/dalmatiner_frontend_version.hrl

compile: version_header
	$(REBAR) compile

clean:
	$(REBAR) clean
	make -C rel/pkg clean
	-rm -r apps/*/ebin

qc:
	$(REBAR) as eqc eqc

eunit: 
	$(REBAR) eunit

test: eunit
	$(REBAR) xref

rel: all
	$(REBAR) as prod release

package: rel
	make -C rel/pkg package

###
### Docs
###
docs:
	$(REBAR) doc

##
## Developer targets
##

xref: all
	$(REBAR) xref

##
## Dialyzer
##
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
       xmerl webtool snmp public_key mnesia eunit syntax_tools compiler edoc

COMBO_PLT = $(HOME)/.dalmatiner_frontend_combo_dialyzer_plt

# DIALYZER_IGNORE="^\(riak_core\|leexinc.hrl\|pokemon_pb.erl\|meck_cover.erl\|meck.erl\|supervisor_pre_r14b04.erl\|webmachine_resource.erl\|uuid.erl\|gen_server2.erl\|folsom_vm_metrics.erl\|protobuffs_compile.erl\)"

check_plt: deps compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin apps/*/ebin

build_plt: deps compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin apps/*/ebin

dialyzer: deps compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) deps/*/ebin apps/*/ebin | grep -v -f dialyzer.mittigate

dialyzer-gui: deps compile
	dialyzer --gui -Wno_return --plt $(COMBO_PLT) deps/*/ebin apps/*/ebin
typer:
	typer --plt $(COMBO_PLT) deps/*/ebin apps/*/ebin

cleanplt:
	@echo
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)

tags:
	find . -name "*.[he]rl" -print | etags -

tree:
	rebar3 tree | grep '|' | sed 's/ (.*//' > tree
