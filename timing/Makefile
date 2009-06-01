PICTURES=$(patsubst %.gnuplot,%.png,$(wildcard *.gnuplot))

gendata:
	make -C
	erlc *.erl

all: $(PICTURES)

clean:
	rm -f $(PICTURES)

%.png: %.gnuplot *.csv
	gnuplot $< > $@
