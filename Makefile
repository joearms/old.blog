.SUFFIXES: .erl .beam

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc  -W $<

# test the expander

all1: code
	erl -pa _src  -s mymd_vsn2 make_blog -s webserver_vsn10 batch	

test1: code
	erl -pa _src  -s mymd_vsn2 test_expand		

code:
	pushd _src; make ; popd

blog: code
	erl -pa _src -s mymd make_blog

https: beams
	erl -s test_server batch

old: beams
	erl -s test1 batch	

beams: ${MODS:%.erl=%.beam}

clean_blog:
	rm -rf 2013 2014 2015 2016
	rm -rf _draft_site
clean:
	rm -rf  *~ erl_crash.dump config
