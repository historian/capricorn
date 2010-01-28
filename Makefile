
SYS_VSNS=$(shell cat rel/capricorn/releases/start_erl.data)
REL_VSN=$(word 2, $(SYS_VSNS))
ERTS_VSN=$(word 1, $(SYS_VSNS))


all: build

build:
	@./rebar compile

release: build
	@./rebar force=1 generate

package: 
	@mkdir -p pkg/tar
	@echo "Release version: $(REL_VSN)"
	@echo "   ERTS version: $(ERTS_VSN)"
	@cp Makefile rel/capricorn/Makefile
	@(cd rel/capricorn ; tar -czf ../../pkg/tar/capricorn-$(REL_VSN).tar.gz ./*)
	@rm -f rel/capricorn/Makefile

build-rpm: package
	@mkdir -p pkg/rpm/BUILD
	@mkdir -p pkg/rpm/RPMS
	@mkdir -p pkg/rpm/SOURCES
	@mkdir -p pkg/rpm/SPECS
	@mkdir -p pkg/rpm/SRPMS
	@cp pkg/tar/capricorn-$(REL_VSN).tar.gz pkg/rpm/SOURCES/capricorn-$(REL_VSN).tar.gz
	@cp capricorn.spec pkg/rpm/SPECS/._capricorn-$(REL_VSN).spec
	@(cd pkg/rpm ; tar -czf ../../pkg/tar/capricorn-$(REL_VSN).src.tar.gz ./*)

clean:
	@./rebar clean

analyze: build
	@./rebar analyze
