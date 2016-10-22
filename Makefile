# A makefile for parse-dataset.scm
#
# Author: Smith Dhumbumroong <zodmaner@gmail.com>

COMPILE = csc
FLAGS = -vf -O5 -static -strict-types

all: parse-dataset

parse-dataset: parse-dataset.scm
	$(COMPILE) $(FLAGS) parse-dataset.scm

clean:
	-rm parse-dataset
