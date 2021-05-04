/*
 *  cool.y
 *              Parser definition for the COOL language.
 *
 */
%{
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "utilities.h"

/* memory */
#define YYINITDEPTH 10000
#define YYMAXDEPTH 10000

extern char *curr_filename;

void yyerror(const char *s);        /*  defined below; called for each parse error */
extern int yylex();           /*  the entry point to the lexer  */

#define YYLTYPE int              /* the type of locations */
#define cool_yylloc curr_lineno
extern int node_lineno;          /* set before constructing a tree node
                                    to whatever you want the line number
                                    for the tree node to be */

/* The default action for locations.  Use the location of the first
   terminal/non-terminal and set the node_lineno to that value. */
#define YYLLOC_DEFAULT(Current, Rhs, N)         \
  Current = Rhs[1];                             \
  node_lineno = Current;

#define SET_NODELOC(Current)  \
  node_lineno = Current;


/************************************************************************/
/*                DONT CHANGE ANYTHING IN THIS SECTION                  */

Program ast_root;             /* the result of the parse  */
Classes parse_results;        /* for use in semantic analysis */
int omerrs = 0;               /* number of errors in lexing and parsing */
%}

%{

// Classes nil_Classes();
// Classes single_Classes(Class_);
// Classes append_Classes(Classes, Classes);
// Features nil_Features();
// Features single_Features(Feature);
// Features append_Features(Features, Features);
// Formals nil_Formals();
// Formals single_Formals(Formal);
// Formals append_Formals(Formals, Formals);
// Expressions nil_Expressions();
// Expressions single_Expressions(Expression);
// Expressions append_Expressions(Expressions, Expressions);
// Cases nil_Cases();
// Cases single_Cases(Case);
// Cases append_Cases(Cases, Cases);
// Program program(Classes);
// Class_ class_(Symbol, Symbol, Features, Symbol);
// Feature method(Symbol, Formals, Symbol, Expression);
// Feature attr(Symbol, Symbol, Expression);
// Formal formal(Symbol, Symbol);
// Case branch(Symbol, Symbol, Expression);
// Expression assign(Symbol, Expression);
// Expression static_dispatch(Expression, Symbol, Symbol, Expressions);
// Expression dispatch(Expression, Symbol, Expressions);
// Expression cond(Expression, Expression, Expression);
// Expression loop(Expression, Expression);
// Expression typcase(Expression, Cases);
// Expression block(Expressions);
// Expression let(Symbol, Symbol, Expression, Expression);
// Expression plus(Expression, Expression);
// Expression sub(Expression, Expression);
// Expression mul(Expression, Expression);
// Expression divide(Expression, Expression);
// Expression neg(Expression);
// Expression lt(Expression, Expression);
// Expression eq(Expression, Expression);
// Expression leq(Expression, Expression);
// Expression comp(Expression);
// Expression int_const(Symbol);
// Expression bool_const(Boolean);
// Expression string_const(Symbol);
// Expression new_(Symbol);
// Expression isvoid(Expression);
// Expression no_expr();
// Expression object(Symbol);

Expression real_expr() {
    int old = node_lineno;
    SET_NODELOC(0);
    Expression res = no_expr();
    SET_NODELOC(old);
    return res;
}


%}


/* A union of all the types that can be the result of parsing actions. Don't change this.*/
%union {
  Boolean boolean;
  Symbol symbol;
  Program program;
  Class_ class_;
  Classes classes;
  Feature feature;
  Features features;
  Formal formal;
  Formals formals;
  Case case_;
  Cases cases;
  Expression expression;
  Expressions expressions;
  char *error_msg;
}

/* 
   Declare the terminals; a few have types for associated lexemes.
   The token ERROR is never used in the parser; thus, it is a parse
   error when the lexer returns it.

   The integer following token declaration is the numeric constant used
   to represent that token internally.  Typically, Bison generates these
   on its own, but we give explicit numbers to prevent version parity
   problems (bison 1.25 and earlier start at 258, later versions -- at
   257)
*/
%token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
%token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
%token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
%token <symbol>  STR_CONST 275 INT_CONST 276 
%token <boolean> BOOL_CONST 277
%token <symbol>  TYPEID 278 OBJECTID 279 
%token ASSIGN 280 NOT 281 LE 282 ERROR 283

/*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
/**************************************************************************/
 
   /* Complete the nonterminal list below, giving a type for the semantic
      value of each non terminal. (See section 3.6 in the bison 
      documentation for details). */

/* Declare types for the grammar's non-terminals. */
%type <program> program
%type <classes> class_list
%type <class_> class
%type <feature> feature
%type <expression> expr
%type <expression> let_list
%type <formal> formal
%type <case_> typcase

/* You will want to change the following line. */
%type <features> feature_list
%type <expressions> comma_expressions
%type <expressions> semicolon_expressions
%type <expressions> parameters
%type <formals> comma_formals
%type <formals> formals
%type <cases> case_list


/* Precedence declarations go here. */
%nonassoc IN
%right ASSIGN
%left NOT
%nonassoc '<' '=' LE
%left '+' '-'
%left '*' '/'
%nonassoc ISVOID
%nonassoc '~'
%nonassoc '@'
%nonassoc '.'



%%
/* 
   Save the root of the abstract syntax tree in a global variable.
*/
program : class_list    { @$ = @1;
                          ast_root = program($1); }
        ;

class_list
        : class                 /* single class */
                { $$ = single_Classes($1);
                  parse_results = $$; }
        | class_list class      /* several classes */
                { $$ = append_Classes($1,single_Classes($2));
                  parse_results = $$; }
        ;

/* If no parent is specified, the class inherits from the Object class. */
class   : CLASS TYPEID '{' feature_list '}' ';'
                { $$ = class_($2,idtable.add_string("Object"),$4,
                              stringtable.add_string(curr_filename)); }
        | CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';'
                { $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
        | CLASS error '{' feature_list '}' ';' { }
        ;


/* Feature list may be empty, but no empty features in list. */

feature_list :  /* empty */    
                { $$ = nil_Features(); }
             |  feature_list feature ';'
                { $$ = append_Features($1, single_Features($2));}
             |  feature_list error ';'
                { }


feature : OBJECTID ':' TYPEID
                { $$ = attr($1, $3, real_expr()); }
        | OBJECTID ':' TYPEID ASSIGN expr
                { $$ = attr($1, $3, $5); }
        | OBJECTID '(' formals ')' ':' TYPEID '{' expr '}'
                { $$ = method($1, $3, $6, $8); }

        | error ASSIGN expr { }
        | error ':' TYPEID { }
        | error '{' expr '}' { }

comma_expressions  : expr
                     { $$ = single_Expressions($1); }
                   | comma_expressions ',' expr
                     { $$ = append_Expressions($1, single_Expressions($3)); }

semicolon_expressions : expr ';'
                        { $$ = single_Expressions($1); }
                      | semicolon_expressions expr ';'
                        { $$ = append_Expressions($1, single_Expressions($2)); }
                      | error ';' { }
                      | error expr ';' { }

parameters : /* empty */
             { $$ = nil_Expressions(); }
           | comma_expressions
             { $$ = $1; }



typcase : OBJECTID ':' TYPEID DARROW expr ';'
          { $$ = branch($1, $3, $5); }
        ;

case_list : typcase
            { $$ = single_Cases($1); }
          | case_list typcase
            { $$ = append_Cases($1,single_Cases($2)); }
          ;

let_list : OBJECTID ':' TYPEID IN expr
           { $$ = let($1, $3, real_expr(), $5); }
         | OBJECTID ':' TYPEID ASSIGN expr IN expr
           { $$ = let($1, $3, $5, $7); }
         | OBJECTID ':' TYPEID ',' let_list
           { $$ = let($1, $3, real_expr(), $5); }
         | OBJECTID ':' TYPEID ASSIGN expr ',' let_list
           { $$ = let($1, $3, $5, $7); }
         | error IN expr { }
         | error ',' let_list { }
         ;

expr : OBJECTID ASSIGN expr
        { $$ = assign($1, $3); }
     | expr '.' OBJECTID '(' parameters ')'
        { $$ = dispatch($1, $3, $5); }
     | expr '@' TYPEID '.' OBJECTID '(' parameters ')'
        { $$ = static_dispatch($1, $3, $5, $7); }
     | OBJECTID '(' parameters ')'
        { $$ = dispatch(object(idtable.add_string("self")), $1, $3); }
     | IF expr THEN expr ELSE expr FI
        { $$ = cond($2, $4, $6); }
     | WHILE expr LOOP expr POOL
        { $$ = loop($2, $4); }
     | '{' semicolon_expressions '}'
        { $$ = block($2); }
     | '{' error '}'
        { }
     | LET let_list
        { $$ = $2; }
     | CASE expr OF case_list ESAC
        { $$ = typcase($2, $4); }
     | NEW TYPEID
        { $$ = new_($2); }
     | ISVOID expr 
        { $$ = isvoid($2); }
     | expr '+' expr
        { $$ = plus($1, $3); }
     | expr '-' expr
        { $$ = sub($1, $3); }
     | expr '*' expr
        { $$ = mul($1, $3); }
     | expr '/' expr
        { $$ = divide($1, $3); }
     | '~' expr
        { $$ = neg($2); }
     | expr '<' expr
        { $$ = lt($1, $3); }
     | expr LE expr
        { $$ = leq($1, $3); }
     | expr '=' expr
        { $$ = eq($1, $3); }
     | NOT expr
        { $$ = comp($2); }
     | '(' expr ')'
        { $$ = $2; }
     | OBJECTID
        { $$ = object($1); }
     | INT_CONST
        { $$ = int_const($1); }
     | STR_CONST
        { $$ = string_const($1); }
     | BOOL_CONST
        { $$ = bool_const($1); }

formal : OBJECTID ':' TYPEID
         { $$ = formal($1, $3); }

comma_formals : formal
                { $$ = single_Formals($1); }
              | comma_formals ',' formal
                { $$ = append_Formals($1, single_Formals($3)); }

formals : /* empty */
          { $$ = nil_Formals(); }
        | comma_formals
          { $$ = $1; }
        ;

/* end of grammar */
%%

/* This function is called automatically when Bison detects a parse error. Don't change this. */
void yyerror(const char *s)
{
  extern int curr_lineno;

  cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
    << s << " at or near ";
  print_cool_token(yychar);
  cerr << endl;
  omerrs++;

  if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
}

