# A makefile for parse-dataset.scm
#
# Author: Smith Dhumbumroong <zodmaner@gmail.com>

SC = csc
FLAGS = -v -O5 -d0 -no-trace -unsafe -fixnum-arithmetic -block -inline -strict-types -specialize

all: parse-dataset

parse-dataset: parse-dataset.scm
	$(SC) $(FLAGS) parse-dataset.scm

clean:
	-rm parse-dataset
