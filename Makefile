REBAR = $(shell pwd)/rebar3

.PHONY: deps rel stagedevrel package version all

all: cp-hooks compile update

update:
	$(REBAR) update

cp-hooks:
	cp hooks/* .git/hooks

version:
	@echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > dalmatiner_frontend.version

version_header: version
	@echo "-define(VERSION, <<\"$(shell cat dalmatiner_frontend.version)\">>)." > apps/dalmatiner_frontend/include/dalmatiner_frontend_version.hrl

compile: update version_header
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

tree:
	rebar3 tree | grep -v '=' | sed 's/ (.*//' > tree

tree-diff: tree
	git diff test -- tree
