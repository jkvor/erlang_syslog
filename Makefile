all: emake

emake:
	erl -make

clean:
	rm -f ebin/*.beam erl_crash.dump