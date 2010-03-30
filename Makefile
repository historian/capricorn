
all: build

build:
	@./rebar compile

release: build
	@./rebar force=1 generate

package: clean
	@echo "Building release: $(REL_VSN)"
	@git tag -a -m "Version $(REL_VSN)" $(REL_VSN)
	@mkdir -p pkg/capricorn-$(REL_VSN)
	@cp .gitignore   pkg/capricorn-$(REL_VSN)/.gitignore
	@cp Makefile     pkg/capricorn-$(REL_VSN)/Makefile
	@cp rebar        pkg/capricorn-$(REL_VSN)/rebar
	@cp rebar.config pkg/capricorn-$(REL_VSN)/rebar.config
	@cp -r lib pkg/capricorn-$(REL_VSN)/lib
	@cp -r rel pkg/capricorn-$(REL_VSN)/rel
	@(cd pkg ; tar -czf capricorn-$(REL_VSN).tar.gz capricorn-$(REL_VSN))
	@rm -rf pkg/capricorn-$(REL_VSN)

clean:
	@./rebar clean

analyze: build
	@./rebar analyze
