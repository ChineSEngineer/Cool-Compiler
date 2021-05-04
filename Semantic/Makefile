SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules
.SUFFIXES: .cc .o

ASSN = 4
CLASS= csgy6413
LIB= 

SRC= semant.cc semant.h cool-tree.h cool-tree.handcode.h good.cl bad.cl README
CSRC= semant-phase.cc symtab_example.cc  handle_flags.cc  ast-lex.cc ast-parse.cc utilities.cc stringtab.cc dumptype.cc tree.cc cool-tree.cc
TSRC= mycoolc mysemant cool-tree.aps
HSRC= tree.h copyright.h stringtab.h list.h cool-io.h cool.h cgen_gc.h ast-parse.h utilities.h cool-parse.h stringtab_functions.h symtab.h
VSRC= testing-harness
CGEN=
HGEN=
LIBS= lexer parser cgen
CFIL= semant.cc ${CSRC} ${CGEN}
LSRC= Makefile
OBJS= ${CFIL:.cc=.o}
OUTPUT= good.output bad.output
SUBMISSIONFILES= good.cl bad.cl semant.h cool-tree.h cool-tree.handcode.h semant.cc README good/* bad/*
ZIPFILE=pa${ASSN}-submission.zip


CPPINCLUDE= -I.

FFLAGS = -d8 -ocool-lex.cc
BFLAGS = -d -v -y -b cool --debug -p cool_yy
ASTBFLAGS = -d -v -y -b ast --debug -p ast_yy

CC=g++
CFLAGS=-g -Wall -Wno-unused ${CPPINCLUDE} -DDEBUG
FLEX=flex ${FFLAGS}
BISON= bison ${BFLAGS}
DEPEND = ${CC} -MM ${CPPINCLUDE}

.PHONY: clean default zip
default: semant

lsource: ${LSRC}

${OUTPUT}: semant
	@rm -f ${OUTPUT}
	./mysemant good.cl >good.output 2>&1 
	-./mysemant bad.cl >bad.output 2>&1 

SEMANT_OBJS := ${filter-out symtab_example.o,${OBJS}}

semant:  ${HSRC} ${SEMANT_OBJS}
	${CC} ${CFLAGS} ${SEMANT_OBJS} ${LIB} -o semant

prep: ${HSRC} ${CFIL} ${VSRC}
	@echo "I hope you ran make clean first!"
	rm -f ../pa${ASSN}.zip
	zip ../pa${ASSN}.zip `find * -type d -prune -o -type f -print`

symtab_example: symtab_example.cc 
	${CC} ${CFLAGS} symtab_example.cc ${LIB} -o symtab_example

.cc.o:
	${CC} ${CFLAGS} -c $<

dotest:	testing-harness parser lexer semant good.cl bad.cl
	@echo "\nRunning semantic checker on good.cl\n"
	-./testing-harness semant good.cl 
	@echo "\nRunning semantic checker on bad.cl\n"
	-./testing-harness semant bad.cl

${LIBS}:
	$(error Please copy your $@ to the current directory)

# These dependencies allow you to get the starting files for
# the assignment.  They will not overwrite a file you already have.

submit-clean: ${OUTPUT}
	-rm -f *.s core ${OBJS} ${CGEN} ${HGEN} lexer *~ parser cgen semant symtab_example

clean :
	-rm -f ${OUTPUT} *.s core ${OBJS} ${HSRC} ${CSRC} ${VSRC} semant cgen symtab_example parser lexer *~ *.a *.o

clean-compile:
	@-rm -f core ${OBJS}

zip:
	@rm -f ${ZIPFILE}
	@zip -MM ${ZIPFILE} ${SUBMISSIONFILES}
	@echo "Zipped files into ${ZIPFILE}"
