
install: get-deps
	@(cd ../erlang && ./rebar compile && ./rebar force=1 generate)

doc:
	@(cd ../erlang && ./rebar doc)

check-deps:
	@(cd ../erlang && ./rebar check-deps)

get-deps:
	@(cd ../erlang && ./rebar get-deps)

delete-deps:
	@(cd ../erlang && ./rebar delete-deps)
