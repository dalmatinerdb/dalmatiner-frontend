VERSION = $(BUILD_NUMBER) # THis does not seem to work at the moment
VERSION ?= 0
COMPONENT = ddb

.PHONY: prepare package

prepare:
	mkdir -p stage/etc
	mkdir -p stage/etc/systemd/system
	mkdir -p stage/usr/bin
	mkdir -p stage/usr/lib/dfe
	mkdir -p stage/var/lib/dfe
	mkdir -p stage/var/log/dfe
	cp -r ../../_build/deb/rel/dalmatinerfe/bin stage/usr/lib/dfe/
	cp -r ../../_build/deb/rel/dalmatinerfe/etc stage/etc/dfe
	cp -r ../../_build/deb/rel/dalmatinerfe/erts-* stage/usr/lib/dfe/
	cp -r ../../_build/deb/rel/dalmatinerfe/lib stage/usr/lib/dfe/
	cp -r ../../_build/deb/rel/dalmatinerfe/releases stage/usr/lib/dfe/
	cp -r ../../_build/deb/rel/dalmatinerfe/share stage/usr/lib/dfe/
	wget https://raw.githubusercontent.com/dalmatinerdb/dqe_idx_pg/master/priv/schema.sql -P stage/var/lib/dfe
	cp -r systemd/dfe.service stage/etc/systemd/system/dfe.service

clean:
	rm -rf ./stage
	rm -f *.deb
