export SHELL = /bin/ksh

TOP := ../..

BUILD = $(TOP)/build
BIN = $(TOP)/bin

.PHONY: all

all: $(BIN)/vscheme $(BIN)/vschemem

$(BIN)/vscheme: vscheme.sml
	mlton -output $@ -verbose 1 $<

$(BIN)/vschemem: vscheme.sml
	mosmlc -o $@ $<

.PHONY: moscow

moscow: $(BIN)/vschemem
