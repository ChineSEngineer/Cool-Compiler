SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules
.SUFFIXES: .cc .o

ASSN = 5
CLASS= csgy6413
LIB= 
SPIM= spim

TRAPHANDLER= trap.handler

SRC= cgen.cc cgen.h cgen_supp.cc cool-tree.h cool-tree.handcode.h emit.h example.cl README
CSRC= cgen-phase.cc utilities.cc stringtab.cc dumptype.cc tree.cc cool-tree.cc ast-lex.cc ast-parse.cc handle_flags.cc 
TSRC= mycoolc
CGEN=
HGEN= 
VSRC= testing-harness
HSRC= copyright.h list.h cool-io.h tree.h cool.h symtab.h cgen_gc.h cool-parse.h utilities.h stringtab_functions.h stringtab.h ast-parse.h
LIBS= lexer parser semant
CFIL= cgen.cc cgen_supp.cc ${CSRC} ${CGEN}
LSRC= Makefile
OBJS= ${CFIL:.cc=.o}
OUTPUT= good.output bad.output
SUBMISSIONFILES= emit.h cgen.cc cgen.h example.cl symtab.h cgen_supp.cc cool-tree.cc cool-tree.h cool-tree.handcode.h README good/*.cl bad/*.cl
ZIPFILE=pa${ASSN}-submission.zip

CPPINCLUDE= -I.


FFLAGS = -d8 -ocool-lex.cc
BFLAGS = -d -v -y -b cool --debug -p cool_yy

CC=g++
CFLAGS=-g -Wall -Wno-unused ${CPPINCLUDE} -Wno-write-strings -DDEBUG
FLEX=flex ${FFLAGS}
BISON= bison ${BFLAGS}
DEPEND = ${CC} -MM ${CPPINCLUDE}

.PHONY: clean default zip
default: cgen

source: ${SRC} ${TSRC} ${LIBS} lsource

lsource: ${LSRC}


${OUTPUT}:	cgen
	@rm -f ${OUTPUT}
	./mycoolc  example.cl >example.output 2>&1 


cgen: ${HSRC} ${OBJS}
	${CC} ${CFLAGS} ${OBJS} ${LIB} -o cgen

prep: ${HSRC} ${CFIL} ${VSRC}
	@echo "I hope you ran make clean first!"
	rm -f ../pa${ASSN}.zip
	zip ../pa${ASSN}.zip `find * -type d -prune -o -type f -print`

%.o: %.cc $(wildcard *.h)
	${CC} ${CFLAGS} -c $<

${TRAPHANDLER}:
	$(error Please make sure that ${TRAPHANDLER} is in your build directory)

example.s: testing-harness parser lexer semant cgen example.cl
	./testing-harness cgen example.cl

dotest: example.s ${TRAPHANDLER}
ifeq (, $(shell which ${SPIM}))
	$(error "No ${SPIM} in PATH, consider doing apt-get install spim")
endif
	${SPIM} -quiet -exception_file ${TRAPHANDLER} -file example.s

${LIBS}:
	$(error Please copy your $@ to the current directory)

# These dependencies allow you to get the starting files for
# the assignment.  They will not overwrite a file you already have.


submit-clean: ${OUTPUT}
	-rm -f *.s core ${OBJS} ${CGEN} ${HGEN} lexer *~ parser cgen semant symtab_example

clean :
	-rm -f ${OUTPUT} *.s core ${OBJS} ${HSRC} ${CSRC} ${VSRC} semant cgen symtab_example parser lexer *~ *.a *.o

clean-compile:
	@-rm -f core ${OBJS} ${LSRC}

zip:
	@rm -f ${ZIPFILE}
	@zip -MM ${ZIPFILE} ${SUBMISSIONFILES}
	@echo "Zipped files into ${ZIPFILE}"
