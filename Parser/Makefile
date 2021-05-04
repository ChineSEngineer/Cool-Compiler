SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules
.SUFFIXES: .cc .o

ASSN = 3
CLASS= csgy6413
LIB= 

SRC= cool.y cool-tree.handcode.h good.cl bad.cl README
CSRC= parser-phase.cc utilities.cc stringtab.cc dumptype.cc \
      tree.cc cool-tree.cc tokens-lex.cc  handle_flags.cc 
TSRC= myparser mycoolc cool-tree.aps
HSRC= cool-parse.h copyright.h tree.h stringtab.h list.h cool-io.h cool.h cool-tree.h utilities.h \
	stringtab_functions.h cgen_gc.h
VSRC= testing-harness
CGEN= cool-parse.cc
HGEN= 
LIBS= lexer semant cgen
CFIL= ${CSRC} ${CGEN}
HFIL= cool-tree.h cool-tree.handcode.h 
LSRC= Makefile
OBJS= ${CFIL:.cc=.o}
OUTPUT= good.output bad.output
SUBMISSIONFILES= cool.y good.cl bad.cl README testcases/*
ZIPFILE=pa${ASSN}-submission.zip


CPPINCLUDE= -I.

BFLAGS = -d -v -y -b cool --debug -p cool_yy

CC=g++
CFLAGS=-g -Wall -Wno-unused -DDEBUG ${CPPINCLUDE}
FLEX=flex ${FFLAGS}
BISON= bison ${BFLAGS}
DEPEND = ${CC} -MM ${CPPINCLUDE}

.PHONY: clean default zip
default: parser

lsource: ${LSRC}

${OUTPUT}:	testing-harness parser good.cl bad.cl
	@rm -f ${OUTPUT}
	./testing-harness parser good.cl >good.output 2>&1 
	-./testing-harness parser bad.cl >bad.output 2>&1 

parser: ${OBJS}
	${CC} ${CFLAGS} ${OBJS} ${LIB} -o parser

prep: ${HSRC} ${CSRC} ${VSRC}
	@echo "I hope you ran make clean first!"
	rm -f ../pa${ASSN}.zip
	zip ../pa${ASSN}.zip `find * -type d -prune -o -type f -print`

.cc.o:
	${CC} ${CFLAGS} -c $<

cool-parse.cc: cool.y
	bison ${BFLAGS} cool.y
	mv -f cool.tab.c cool-parse.cc

dotest:	testing-harness parser lexer good.cl bad.cl
	@echo "\nRunning parser on good.cl\n"
	-./testing-harness parser good.cl 
	@echo "\nRunning parser on bad.cl\n"
	-./testing-harness parser bad.cl

${LIBS}:
	$(error Please copy your $@ to the current directory)

# These dependencies allow you to get the starting files for
# the assignment.  They will not overwrite a file you already have.

${SRC} :								

${LSRC} :

${VSRC} :
	cp ../../src/$@ .

${CSRC}: ${HSRC}
	cp ../../src/$@ .

${HSRC}:
	cp ../../src/$@ .

submit-clean: ${OUTPUT}
	-rm -f *.s core ${OBJS} ${CGEN} ${HGEN} lexer *~ parser cgen semant

clean:
	-rm -f ${OUTPUT} cool.output *.s core ${OBJS} ${CGEN} ${HGEN} lexer parser cgen semant *~ *.a *.o  cool.tab.h cool.tab.c ${HSRC} ${CSRC} ${VSRC}

clean-compile:
	@-rm -f core ${OBJS} ${CGEN} ${HGEN} ${LSRC}

zip:
	@rm -f ${ZIPFILE}
	@zip -MM ${ZIPFILE} ${SUBMISSIONFILES}
	@echo "Zipped files into ${ZIPFILE}"
