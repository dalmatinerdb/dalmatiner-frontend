VERSION = $(BUILD_NUMBER) # THis does not seem to work at the moment
VERSION ?= 0
COMPONENT = dfe

.PHONY: prepare package

prepare:
	mkdir -p stage/data/dalmatinerfe
	mkdir -p stage/etc/systemd/system
	mkdir -p stage/usr/bin
	mkdir -p stage/usr/lib/dfe
	mkdir -p stage/var/lib/dfe
	mkdir -p stage/var/log/dfe
	mkdir -p stage/DEBIAN
	cp -r ../../_build/deb/rel/dalmatinerfe/bin stage/usr/lib/dfe/
	cp -r ../../_build/deb/rel/dalmatinerfe/etc stage/data/dalmatinerfe/etc/
	cp -r ../../_build/deb/rel/dalmatinerfe/erts-* stage/usr/lib/dfe/
	cp -r ../../_build/deb/rel/dalmatinerfe/lib stage/usr/lib/dfe/
	cp -r ../../_build/deb/rel/dalmatinerfe/releases stage/usr/lib/dfe/
	cp -r ../../_build/deb/rel/dalmatinerfe/share stage/usr/lib/dfe/
	cp -r systemd/dalmatinerfe.service stage/etc/systemd/system/dalmatinerfe.service
	cp control stage/DEBIAN/	
	cp preinst stage/DEBIAN/
	cp postinst stage/DEBIAN/
	chmod 555 stage/DEBIAN/preinst
	chmod 555 stage/DEBIAN/postinst

package:
	dpkg-deb --build stage

clean:
	rm -rf ./stage
	rm -f *.deb
