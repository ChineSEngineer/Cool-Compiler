SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules
.SUFFIXES: .cc .o

ASSN = 2
CLASS= csgy6413
LIB= 

SRC= cool.flex test.cl README 
CSRC= lextest.cc utilities.cc stringtab.cc handle_flags.cc
TSRC= mycoolc
HSRC= copyright.h cool-io.h tree.h stringtab.h list.h utilities.h stringtab_functions.h cgen_gc.h cool-parse.h
VSRC= testing-harness
CGEN= cool-lex.cc
HGEN= cool-parse.h
LIBS= parser semant cgen
CFIL= ${CSRC} ${CGEN}
LSRC= Makefile
OBJS= ${CFIL:.cc=.o}
OUTPUT= test.output
SUBMISSIONFILES= cool.flex test.cl README
ZIPFILE=pa${ASSN}-submission.zip


CPPINCLUDE= -I.


FFLAGS= -d -ocool-lex.cc

CC=g++
CFLAGS= -g -Wall -Wno-unused ${CPPINCLUDE}
FLEX=flex ${FFLAGS}
DEPEND = ${CC} -MM ${CPPINCLUDE}

.PHONY: clean default zip
default: lexer

source : ${SRC} ${TSRC} ${LSRC} ${LIBS} lsource

lsource: ${LSRC}

${OUTPUT}:	lexer test.cl
	@rm -f test.output
	-./lexer test.cl >test.output 2>&1 

lexer: ${OBJS} ${SRC}
	${CC} ${CFLAGS} ${OBJS} ${LIB} -o lexer

prep: ${HSRC} ${CSRC}
	rm -f ../pa${ASSN}.zip
	zip ../pa${ASSN}.zip `find * -type d -prune -o -type f -print`

.cc.o:
	${CC} ${CFLAGS} -c $<

cool-lex.cc: cool.flex 
	${FLEX} cool.flex

dotest:	testing-harness lexer test.cl
	@echo "\nRunning lexer on test.cl\n"
	-./testing-harness lexer test.cl 


${LIBS}:
	

# These dependencies allow you to get the starting files for
# the assignment.  They will not overwrite a file you already have.

${SRC} :

${LSRC} :

${VSRC} :
	cp ../../src/$@ .
	
${HSRC} :
	cp  ../../src/$@ .
	
${CSRC}: ${HSRC}
	cp  ../../src/$@ .

submit-clean: ${OUTPUT}
	-rm -f *.s core ${OBJS} lexer cool-lex.cc *~ parser cgen semant

clean:
	-rm -f ${OUTPUT} *.s core ${OBJS} ${VSRC} lexer cool-lex.cc *~ parser cgen semant ${CSRC} ${HSRC}

clean-compile:
	@-rm -f core ${OBJS} cool-lex.cc ${LSRC}

zip:
	@rm -f ${ZIPFILE}
	@zip -MM ${ZIPFILE} ${SUBMISSIONFILES}
	@echo "Zipped files into ${ZIPFILE}"

