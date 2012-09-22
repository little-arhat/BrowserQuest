
all:
	./rebar skip_deps=true compile

deps:
	./rebae get-deps compile

run:all
	erl -pa ebin -pa deps/*/ebin -s reloader -s bq
