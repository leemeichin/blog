.PHONY: clean package

STACK_INSTALL_PATH = $(shell stack path --local-install-root)

clean:
	stack clean
	rm -f debian/kamelasa.substvars debian/kamelasa.debhelper.log

package: clean
	dpkg-buildpackage -b -us -uc

kamelasa.hs:
	stack build
