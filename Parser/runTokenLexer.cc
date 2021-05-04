#include "cool-parse.h"
#include <stdio.h>
#include <stdlib.h>
#include "cool-io.h"
#include <unistd.h>
int curr_lineno;
#define yylval          cool_yylval
YYSTYPE yylval;
Program ast_root;
int omerrs = 0;

extern int cool_yylex(void);

int cool_yyparse() {
    int res;
    while(res = cool_yylex()) {
        cout << res << endl;
    }
}
int cool_yydebug = 0;

void handle_flags(int argc, char *argv[]) {
}

// g++ tokens-lex.cc stringtab.cc dumptype.cc utilities.cc tree.cc runTokenLexer.cc parser-phase.cc cool-tree.cc
