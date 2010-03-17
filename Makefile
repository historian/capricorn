
SYS_VSNS=$(shell cat rel/capricorn/releases/start_erl.data)
REL_VSN=$(word 2, $(SYS_VSNS))
ERTS_VSN=$(word 1, $(SYS_VSNS))


all: build

build:
	@./rebar compile

release: build
	@./rebar force=1 generate

package: release package-tar
	@echo "Release version: $(REL_VSN)"
	@echo "   ERTS version: $(ERTS_VSN)"

package-tar:
	@mkdir -p pkg/tar
	@(cd rel/capricorn ; tar -czf ../../pkg/tar/capricorn-$(REL_VSN).embedded.darwin.tar.gz ./*)

clean:
	@./rebar clean

analyze: build
	@./rebar analyze
