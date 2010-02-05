
SYS_VSNS=$(shell cat rel/capricorn/releases/start_erl.data)
REL_VSN=$(word 2, $(SYS_VSNS))
ERTS_VSN=$(word 1, $(SYS_VSNS))


all: build

build:
	@./rebar compile

release: build
	@./rebar force=1 generate

package: release package-tar package-faxien
	@echo "Release version: $(REL_VSN)"
	@echo "   ERTS version: $(ERTS_VSN)"

package-tar:
	@mkdir -p pkg/tar
	@(cd rel/capricorn ; tar -czf ../../pkg/tar/capricorn-$(REL_VSN).embedded.darwin.tar.gz ./*)

package-faxien:
	@mkdir -p pkg/faxien
	@rm -rf pkg/faxien/capricorn-$(REL_VSN)
	@cp -rf rel/capricorn pkg/faxien/capricorn-$(REL_VSN)
	@rm -rf pkg/faxien/capricorn-$(REL_VSN)/etc pkg/faxien/capricorn-$(REL_VSN)/var pkg/faxien/capricorn-$(REL_VSN)/erts-5.7.4
	@rm -f  pkg/faxien/capricorn-$(REL_VSN)/releases/$(REL_VSN)/start_clean.*
	@rm -f  pkg/faxien/capricorn-$(REL_VSN)/releases/start_erl.data
	@mv     pkg/faxien/capricorn-$(REL_VSN)/releases/$(REL_VSN) pkg/faxien/capricorn-$(REL_VSN)/releases/capricorn-$(REL_VSN)
	@(cd pkg/faxien/capricorn-$(REL_VSN)/lib ; for ez in *.ez ; do unzip $$ez; done ; rm *.ez)
	@(cd pkg/faxien ; tar -czf ../tar/capricorn-$(REL_VSN).faxien.tar.gz capricorn-$(REL_VSN))

install: package
	@echo "Installing with faxien..."
	@sudo faxien install-release pkg/tar/capricorn-$(REL_VSN).faxien.tar.gz

clean:
	@./rebar clean

analyze: build
	@./rebar analyze
