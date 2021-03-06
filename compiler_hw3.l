/* Definition section */
%{
    #include "common.h"
    #include "y.tab.h"	/* header file generated by bison */
    extern YYSTYPE yylval;

    #define YY_NO_UNPUT
    #define YY_NO_INPUT
%}


/* Define regular expression label */
letter  [a-zA-Z_]
digit [0-9]
id {letter}+({letter}|{digit})*
inumber {digit}+
fnumber ({digit}*\.{digit}+)
newline		[\n]

%x CMT
%x STR
%option yylineno

/* Rules section */
%%

"/*"        { BEGIN(CMT); }
<CMT>"*/"   { BEGIN(INITIAL); }
<CMT>\n     {;}
<CMT>.      {;}

"//".*      {;}

"\""        { BEGIN(STR); }
<STR>"\""   { BEGIN(INITIAL); }
<STR>[^\"]* { yylval.s_val = strdup(yytext);
                return STRING_LIT;
            }
			
"+"         { return ADD; }
"-"         { return SUB; }
"*"         { return MUL; }
"/"         { return QUO; }
"%"         { return REM; }
"++"        { return INC; }
"--"        { return DEC; }
">"         { return GTR; }
"<"         { return LSS; }
">="        { return GEQ; }
"<="        { return LEQ; }
"=="        { return EQL; }
"!="        { return NEQ; }
"="         { return ASSIGN; }
"+="        { return ADD_ASSIGN; }
"-="        { return SUB_ASSIGN; }
"*="        { return MUL_ASSIGN; }
"/="        { return QUO_ASSIGN; }
"%="        { return REM_ASSIGN; }
"&&"        { return AND; }
"||"        { return OR; }
"!"         { return NOT; }
"("         { return LPAREN; }
")"         { return RPAREN; }
"["         { return LBRACK; }
"]"         { return RBRACK; }
"{"         { return LBRACE; }
"}"         { return RBRACE; }

"int"       { return INT; }
"float"     { return FLOAT; }
"bool"      { return BOOL; }
"string"    { return STRING; }
"print"     { return PRINT; }
"if"        { return IF; }
"else"      { return ELSE; }
"while"     { return WHILE; }
"for"       { return FOR; }
"true"      { yylval.s_val = strdup(yytext);
                return TRUE; }
"false"     { yylval.s_val = strdup(yytext);
                return FALSE; }

{inumber}   { yylval.i_val = atoi(yytext);
                return INT_LIT;
            }
{fnumber}   { yylval.f_val = atof(yytext);
                return FLOAT_LIT;
            }

{id}        { yylval.s_val = strdup(yytext);
                return IDENT;
            }
[;]         { return SEMICOLON; }
[\n]        {;}
<<EOF>>     { static int once = 0;
                if (once++) {
                    yyterminate();
                }
            }
[ \t]+      {;}
.           {;}
%%
/*  C Code section */
int yywrap(void)
{
    return 1;
}