all: 
	erl -make
	erl -pa ebin/

clean:
	rm -f ebin/*.beam *.beam *.dump

run_tests:
	erl -make
	erl -noshell -eval "eunit:test(test_client), halt()"
