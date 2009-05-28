SOURCES=$(wildcard *.erl)
TARGETS=$(patsubst %.erl,%.beam,$(SOURCES))

all: $(TARGETS)

clean:
	rm -f *.beam

%.beam: %.erl
	erlc $<
