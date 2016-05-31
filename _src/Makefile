.SUFFIXES: .erl .beam

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc  -W $<

all: ehe_parser_vsn5.beam beams 

test_ehe: all
	erl -s ehe_vsn5 test

ehe_parser_vsn5.erl: ehe_parser_vsn5.yrl
	erlc ehe_parser_vsn5.yrl

beams: ${MODS:%.erl=%.beam}

clean:
	rm -rf  *.beam ehe_parser_vsn5.erl *~ erl_crash.dump
	rm -rf ehe_tests_vsn5.html

