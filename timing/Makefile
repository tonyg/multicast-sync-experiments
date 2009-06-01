PICTURES=$(patsubst %.gnuplot,%.png,$(wildcard *.gnuplot))

all: $(PICTURES)

regendata:
	make -C ..
	erlc linear_intervals.erl debughelper.erl
	erlc -DSET=gb_sets t.erl && erl -smp disable -pa .. -eval 't:t_all(), init:stop()'
	erlc -DSET=linear_intervals t.erl && erl -smp disable -pa .. -eval 't:t_all(), init:stop()'
	erlc -DSET=intervals t.erl && erl -smp disable -pa .. -eval 't:t_all(), init:stop()'

clean:
	rm -f $(PICTURES)
	rm linear_intervals.beam debughelper.beam t.beam

%.png: %.gnuplot data/*.csv
	gnuplot $< > $@
