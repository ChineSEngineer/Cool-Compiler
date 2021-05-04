/*
 *  The scanner definition for COOL.  */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */

%option noyywrap

%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
        if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
                YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
char translate_escape(const char* text) {
    if (strcmp(text, "\\\n") == 0) {
        return '\n';
    }
    if (strcmp(text, "\\n") == 0) {
        return '\n';
    }
    if (strcmp(text, "\\t") == 0) {
        return '\t';
    }
    if (strcmp(text, "\\b") == 0) {
        return '\b';
    }
    if (strcmp(text, "\\f") == 0) {
        return '\f';
    }
    if (strcmp(text, "\\\"") == 0) {
        return '\"';
    }
    return *(text + 1);
}


const char* STRING_TOO_LONG      = "String constant too long";
const char* UNEXPECT_END_COMMENT = "Unmatched *)";
const char* EOF_IN_COMMENT       = "EOF in comment";
const char* EOF_IN_STRING        = "EOF in string constant";
const char* UNTERMINATED_STRING  = "Unterminated string constant";
const char* NULL_IN_STRING       = "String contains null character.";
int comment_state = 0;
std::string tmp_string;
bool string_error_reported;


%}

/*
 * Define names for regular expressions here.
 */

DARROW          =>
ASSIGN          <-
LE              <=
COMMENT_START   \(\*
COMMENT_END     \*\)
DOUBLE_DASH     --
UNARY           [;:{}()+-*/=<.~,@]
ESCAPE_CHAR     \\\n|\\n|\\t|\\b|\\f|\\\"|\\.
SPACE           [\ \t\f\b]

%x              COMMENT_STATE
%x              DOUBLE_DASH_COMMENT_STATE
%x              STRING_STATE
%x              ERROR_STATE


%%

 /*
  *  Nested comments
  */
{COMMENT_END}           { yylval.error_msg = UNEXPECT_END_COMMENT; return ERROR; }
{DOUBLE_DASH}           { BEGIN(DOUBLE_DASH_COMMENT_STATE); }

<DOUBLE_DASH_COMMENT_STATE>{
    \n                  { curr_lineno++; BEGIN(INITIAL); }
    <<EOF>>             { yylval.error_msg = EOF_IN_COMMENT; BEGIN(INITIAL); return ERROR; }
    .                   {}
}

<COMMENT_STATE,INITIAL>{
    {COMMENT_START}     { 
                            if (comment_state == 0) {
                                BEGIN(COMMENT_STATE);
                            }
                            comment_state++;
                        }
    \n                  { curr_lineno++; }
}

<COMMENT_STATE>{
    {COMMENT_END}       {
                            comment_state--;
                            if (comment_state == 0) {
                                BEGIN(INITIAL);
                            }
                        } 
    <<EOF>>             { yylval.error_msg = EOF_IN_COMMENT; BEGIN(INITIAL); return ERROR; }
    .                   {}
}


 /*
  *  The multiple-character operators.
  */
{DARROW}                { return (DARROW); }
{ASSIGN}                { return (ASSIGN); }
{LE}                    { return (LE); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
class                   { return (CLASS); }
else                    { return (ELSE); }
fi                      { return (FI); }
if                      { return (IF); }
in                      { return (IN); }
inherits                { return (INHERITS); }
let                     { return (LET); }
loop                    { return (LOOP); }
pool                    { return (POOL); }
then                    { return (THEN); }
while                   { return (WHILE); }
case                    { return (CASE); }
esac                    { return (ESAC); }
of                      { return (OF); }
new                     { return (NEW); }
not                     { return (NOT); }
isvoid                  { return (ISVOID); }

t[Rr][Uu][Ee]           { yylval.boolean = true; return BOOL_CONST; }
f[Aa][Ll][Ss][Ee]       { yylval.boolean = false; return BOOL_CONST; }
[A-Z][a-zA-Z0-9_]*      { yylval.symbol = idtable.add_string(yytext ,yyleng);return (TYPEID); }
[a-z][a-zA-Z0-9_]*      { yylval.symbol = idtable.add_string(yytext ,yyleng);return (OBJECTID); }
[0-9]+                  { yylval.symbol = inttable.add_string(yytext ,yyleng);return (INT_CONST); }
[;:{}()+-/=<.~,@\*]     { return *yytext; }


 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
\"                      { tmp_string.clear(); string_error_reported = false; BEGIN(STRING_STATE); }
<STRING_STATE>{
    \"                  {
                            BEGIN(INITIAL);
                            if (!string_error_reported) {
                                yylval.symbol = stringtable.add_string(tmp_string.c_str(), tmp_string.size());
                                return STR_CONST; 
                            }
                        }
    {ESCAPE_CHAR}       {
                            if (strcmp(yytext, "\\\n") == 0) {
                                curr_lineno++;
                            }
                            if (!string_error_reported) {
                                if (tmp_string.size() < 1024) {
                                    tmp_string.push_back(translate_escape(yytext));
                                } else {
                                    yylval.error_msg = STRING_TOO_LONG;
                                    string_error_reported = true;
                                    tmp_string.clear();
                                    return ERROR;
                                }
                            }
                        }
    <<EOF>>             {
                            yylval.error_msg = EOF_IN_STRING;
                            tmp_string.clear();
                            BEGIN(INITIAL);
                            return ERROR;
                        }
    \n                  {
                            curr_lineno++;
                            tmp_string.clear();
                            BEGIN(INITIAL);
                            if (!string_error_reported) {
                                yylval.error_msg = UNTERMINATED_STRING;
                                return ERROR;
                            } else {
                                string_error_reported = false;
                            }
                        }
    \0                  {
                            if (!string_error_reported) {
                                yylval.error_msg = NULL_IN_STRING;
                                string_error_reported = true;
                                tmp_string.clear();
                                return ERROR;
                            }
                        }
    .                   {
                            if (!string_error_reported) {
                                if (tmp_string.size() < 1024) {
                                    tmp_string.push_back(*yytext);
                                } else {
                                    yylval.error_msg = STRING_TOO_LONG;
                                    string_error_reported = true;
                                    tmp_string.clear();
                                    return ERROR;
                                }
                            }
                        }
}

{SPACE}                 {}
.                       { yylval.error_msg = yytext; return ERROR; }
%%
