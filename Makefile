APP=dalmatiner_frontend
REBAR = $(shell pwd)/rebar3

.PHONY: deps rel stagedevrel package version all tree

all: version_header compile

include fifo.mk

version:
	@echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > dalmatiner_frontend.version

version_header: version
	@echo "-define(VERSION, <<\"$(shell cat dalmatiner_frontend.version)\">>)." > apps/dalmatiner_frontend/include/dalmatiner_frontend_version.hrl

clean:
	$(REBAR) clean
	make -C rel/pkg clean
	make -C rel/deb clean
	-rm -r apps/*/ebin

rel: update
	$(REBAR) as prod compile
	$(REBAR) as prod release

package: rel
	make -C rel/pkg package

deb-clean: 
	make -C rel/deb clean

deb-prepare: update
	$(REBAR) as deb compile
	$(REBAR) as deb release
	make -C rel/deb prepare
