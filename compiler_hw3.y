/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    #define codegen(...) \
        do { \
            for (int i = 0; i < INDENT; i++) { \
                fprintf(fout, "\t"); \
            } \
            fprintf(fout, __VA_ARGS__); \
        } while (0)

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;
    
    /* Other global variables */
    FILE *fout = NULL;
    bool HAS_ERROR = false;
    int INDENT = 0;
    int LabelCount = 0;
    int IfLabelCount[50] = {0};
    int WhileLabelCount[50] = {0};
    int ForCount = 0;
    int IsExit[50] = {0};
    int addr, size;
    bool IS_ARRAY;

    typedef struct node { 
        char *Name;
        char *Type;
        int Address;
        int Lineno;
        char *ElementType;
    } Node;

    Node table[50][50];
    int count[50] = {0};
    int scope = 0;
    int totalNum = 0;
    
    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

    /* Symbol table function - you can add new function if needed. */
    //static void create_symbol();
    static void insert_symbol(char *name, char *type, char *element) {
        for(int i = 0; i < count[scope]; i++) {
            if(strcmp(table[scope][i].Name, name) == 0) {
                printf("error:%d: %s redeclared in this block. previous declaration at line %d\n", yylineno, name, table[scope][i].Lineno);
                return;
            }
        }
        table[scope][count[scope]].Name =  name;
        table[scope][count[scope]].Type = type;
        table[scope][count[scope]].Address = totalNum;
        table[scope][count[scope]].Lineno = yylineno;
        table[scope][count[scope]].ElementType = element;
        printf("> Insert {%s} into symbol table (scope level: %d)\n", name, scope);
        totalNum++;
        count[scope]++;
    }
    static char* lookup_symbol(char *name) {
        for(int i = scope; i >= 0; i--) {
            for(int j = 0; j < count[i]; j++) {
                if(strcmp(table[i][j].Name, name) == 0) {
                    printf("IDENT (name=%s, address=%d)\n", name, table[i][j].Address);
                    if(strcmp(table[i][j].Type, "array") == 0)
                        return table[i][j].ElementType;
                    else {
                        return table[i][j].Type;
                    }
                }
            }
        }
        return "-1";
    }
    static void dump_symbol() {
        printf("> Dump symbol table (scope level: %d)\n", scope);
        printf("%-10s%-10s%-10s%-10s%-10s%s\n", "Index", "Name", "Type", "Address", "Lineno", "Element type");
        for(int i = 0; i < count[scope]; i++) {
            printf("%-10d%-10s%-10s%-10d%-10d%s\n", i, table[scope][i].Name, table[scope][i].Type, table[scope][i].Address, table[scope][i].Lineno, table[scope][i].ElementType);
        }
        count[scope] = 0;
    }
    static void store_load(char *name, int func) {
        for(int i = scope; i >= 0; i--) {
            for(int j = 0; j < count[i]; j++) {
                if(strcmp(table[i][j].Name, name) == 0) {
                    if(strcmp(table[i][j].Type, "int") == 0) {
                        if(func == 0) 
                            codegen("istore %d\n\n", table[i][j].Address);
                        else  
                            codegen("iload %d\n", table[i][j].Address);
                    }
                    else if(strcmp(table[i][j].Type, "float") == 0) { 
                        if(func == 0)
                            codegen("fstore %d\n\n", table[i][j].Address);
                        else 
                            codegen("fload %d\n", table[i][j].Address);
                    }
                    else if(strcmp(table[i][j].Type, "string") == 0) {
                        if(func == 0)
                            codegen("astore %d\n\n", table[i][j].Address);
                        else 
                            codegen("aload %d\n", table[i][j].Address);
                    }
                    else if(strcmp(table[i][j].Type, "bool") == 0) {
                        if(func == 0)
                            codegen("istore %d\n\n", table[i][j].Address);
                        else
                            codegen("iload %d\n", table[i][j].Address);
                    }
                    else {
                        addr = table[i][j].Address;
                        if(func == 0) { 
                            if(strcmp(table[i][j].ElementType, "int") == 0)
                                codegen("iastore\n\n");
                            else if(strcmp(table[i][j].ElementType, "float") == 0)
                                codegen("fastore\n\n");
                        }
                        else if(func == 1)
                            codegen("aload %d\n", table[i][j].Address);
                        else 
                            codegen("astore %d\n\n", table[i][j].Address);
                    }
                    return;
                }
            }
        }
    }
    static bool is_array(char *name) {
        for(int i = scope; scope >= 0; i--) {
            for(int j = 0; j < count[i]; j++) {
                if(strcmp(table[i][j].Name, name) == 0) {
                    if(strcmp(table[i][j].Type, "array") == 0)
                        return true;
                    else 
                        return false;
                }
            }
        }
        return false;
    }
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
}
/* Token without return */
%token <s_val> INT FLOAT BOOL STRING 
%token <s_val> IDENT
%token SEMICOLON
%token ADD SUB MUL QUO REM INC DEC
%token EQL NEQ LSS LEQ GTR GEQ
%token ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token OR AND NOT
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token IF ELSE WHILE FOR PRINT TRUE FALSE

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT

/* Nonterminal with return, which need to sepcify type */
%type <s_val> assign_op unary_op Type TypeName Literal
%type <s_val> Expression UnaryExpr AssignmentExpr MulExpr AddExpr CmpExpr AndExpr OrExpr IndexExpr
%type <s_val> Operand PrimaryExpr PostfixExpr ConversionExpr IfStmt

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : StatementList
;

Type
    : TypeName 
;

TypeName
    : INT { $$ = "int"; }
    | FLOAT { $$ = "float"; }
    | STRING { $$ = "string"; }
    | BOOL { $$ = "bool"; }
;

ConversionExpr
    : UnaryExpr
    | LPAREN Type RPAREN ConversionExpr 
        {   
            if(($4 == "float" || lookup_symbol($4) == "float") && $2 == "int")
                codegen("f2i\n");
            else if(($4 == "int" || lookup_symbol($4) == "int") && $2 == "float")
                codegen("i2f\n");

            if($4 == "float") printf("F"); else printf("I");
            printf(" to ") ;
            if($2 == "float") printf("F\n"); else printf("I\n");
            $$ = $2;
        }
;

PostfixExpr
    : PrimaryExpr
    | PostfixExpr INC 
        { 
            printf("INC\n"); 
            if(lookup_symbol($1) == "int") {
                codegen("ldc 1\n");
                codegen("iadd\n");
            }
            else { 
                codegen("ldc 1.0\n");
                codegen("fadd\n");
            }
            store_load($1, 0);
        }
    | PostfixExpr DEC 
        { 
            printf("DEC\n"); 
            if(lookup_symbol($1) == "int") {
                codegen("ldc 1\n");
                codegen("isub\n");
            }
            else {
                codegen("ldc 1.0\n");
                codegen("fsub\n");
            }
            store_load($1, 0);
        }
;

UnaryExpr
    : PostfixExpr
    | unary_op ConversionExpr 
        {   
            printf("%s\n", $1);
            $$ = $2; 
            if($1 == "NEG") { 
                if($2 == "int" || lookup_symbol($2) == "int")
                    codegen("ineg\n");
                else
                    codegen("fneg\n");
            }
            else if($1 == "NOT")
                codegen("ixor\n");
        }
;

unary_op
    : ADD { $$ = "POS"; }
    | SUB { $$ = "NEG"; }
    | NOT { codegen("iconst_1\n"); $$ = "NOT"; }
;

Expression 
    : AssignmentExpr
;

MulExpr
    : ConversionExpr
    | MulExpr MUL ConversionExpr 
        { 
            printf("MUL\n");
            if($1 == "float" || lookup_symbol($1) == "float" || $3 == "float" || lookup_symbol($3) == "float")
                codegen("fmul\n");
            else 
                codegen("imul\n");
        }
    | MulExpr QUO ConversionExpr 
        { 
            printf("QUO\n");
            if($1 == "float" || lookup_symbol($1) == "float" || $3 == "float" || lookup_symbol($3) == "float")
                codegen("fdiv\n");
            else 
                codegen("idiv\n");
        }
    | MulExpr REM ConversionExpr 
        { 
            printf("REM\n");
            if(($1 != "int" && lookup_symbol($1) != "int") || ($3 != "int" && lookup_symbol($3) != "int")) {
                printf("error:%d: invalid operation: (operator REM not defined on float)\n", yylineno);
                HAS_ERROR = true;
            }
            codegen("irem\n");
        }
;

AddExpr
    : MulExpr
    | AddExpr ADD MulExpr 
        {   
            printf("ADD\n");
            if($1 != $3 && $1 != lookup_symbol($3) && lookup_symbol($1) != $3 && lookup_symbol($1) != lookup_symbol($3)) {
                printf("error:%d: invalid operation: ADD (mismatched types %s and %s)\n", yylineno, $1, $3); 
                HAS_ERROR = true;   
            }
            else if($1 == "float" || lookup_symbol($1) == "float" || $3 == "float" || lookup_symbol($3) == "float")
                codegen("fadd\n");
            else 
                codegen("iadd\n");
        }
    | AddExpr SUB MulExpr 
        {   
            printf("SUB\n");
            if($1 != $3 && $1 != lookup_symbol($3) && lookup_symbol($1) != $3 && lookup_symbol($1) != lookup_symbol($3)) {
                printf("error:%d: invalid operation: SUB (mismatched types %s and %s)\n", yylineno, $1, $3); 
                HAS_ERROR = true;
            }
            else if($1 == "float" || lookup_symbol($1) == "float" || $3 == "float" || lookup_symbol($3) == "float")
                codegen("fsub\n");
            else 
                codegen("isub\n");
        }
;

CmpExpr
    : AddExpr
    | CmpExpr EQL AddExpr 
        { 
            printf("EQL\n"); 
            $$ = "bool"; 
            if(($1 == "int" || lookup_symbol($1) == "int") && ($3 == "int" || lookup_symbol($3) == "int")) 
                codegen("isub\n");
            else 
                codegen("fcmpl\n");
            codegen("ifeq L_cmp_%d\n", LabelCount);
            codegen("iconst_0\n");
            codegen("goto L_cmp_%d\n", LabelCount+1);
            INDENT--;
            codegen("L_cmp_%d:\n", LabelCount);
            INDENT++;
            codegen("iconst_1\n");
            INDENT--;
            codegen("L_cmp_%d:\n\n", LabelCount+1);
            INDENT++;
            LabelCount += 2;
        }
    | CmpExpr NEQ AddExpr 
        { 
            printf("NEQ\n"); 
            $$ = "bool"; 
            if(($1 == "int" || lookup_symbol($1) == "int") && ($3 == "int" || lookup_symbol($3) == "int")) 
                codegen("isub\n");
            else 
                codegen("fcmpl\n");
            codegen("ifne L_cmp_%d\n", LabelCount);
            codegen("iconst_0\n");
            codegen("goto L_cmp_%d\n", LabelCount+1);
            INDENT--;
            codegen("L_cmp_%d:\n", LabelCount);
            INDENT++;
            codegen("iconst_1\n");
            INDENT--;
            codegen("L_cmp_%d:\n\n", LabelCount+1);
            INDENT++;
            LabelCount += 2;
        }
    | CmpExpr GTR AddExpr 
        {    
            printf("GTR\n");
            $$ = "bool"; 
            if(($1 == "int" || lookup_symbol($1) == "int") && ($3 == "int" || lookup_symbol($3) == "int")) 
                codegen("isub\n");
            else 
                codegen("fcmpl\n");
            codegen("ifgt L_cmp_%d\n", LabelCount);
            codegen("iconst_0\n");
            codegen("goto L_cmp_%d\n", LabelCount+1);
            INDENT--;
            codegen("L_cmp_%d:\n", LabelCount);
            INDENT++;
            codegen("iconst_1\n");
            INDENT--;
            codegen("L_cmp_%d:\n\n", LabelCount+1);
            INDENT++;
            LabelCount += 2;
        }
    | CmpExpr LSS AddExpr 
        { 
            printf("LSS\n");
            $$ = "bool"; 
            if(($1 == "int" || lookup_symbol($1) == "int") && ($3 == "int" || lookup_symbol($3) == "int")) 
                codegen("isub\n");
            else 
                codegen("fcmpl\n");
            codegen("iflt L_cmp_%d\n", LabelCount);
            codegen("iconst_0\n");
            codegen("goto L_cmp_%d\n", LabelCount+1);
            INDENT--;
            codegen("L_cmp_%d:\n", LabelCount);
            INDENT++;
            codegen("iconst_1\n");
            INDENT--;
            codegen("L_cmp_%d:\n\n", LabelCount+1);
            INDENT++;
            LabelCount += 2;
        }
    | CmpExpr GEQ AddExpr 
        { 
            printf("GEQ\n"); 
            $$ = "bool"; 
            if(($1 == "int" || lookup_symbol($1) == "int") && ($3 == "int" || lookup_symbol($3) == "int")) 
                codegen("isub\n");
            else 
                codegen("fcmpl\n");
            codegen("ifge L_cmp_%d\n", LabelCount);
            codegen("iconst_0\n");
            codegen("goto L_cmp_%d\n", LabelCount+1);
            INDENT--;
            codegen("L_cmp_%d:\n", LabelCount);
            INDENT++;
            codegen("iconst_1\n");
            INDENT--;
            codegen("L_cmp_%d:\n\n", LabelCount+1);
            INDENT++;
            LabelCount += 2;
        }
    | CmpExpr LEQ AddExpr 
        { 
            printf("LEQ\n"); 
            $$ = "bool"; 
            if(($1 == "int" || lookup_symbol($1) == "int") && ($3 == "int" || lookup_symbol($3) == "int")) 
                codegen("isub\n");
            else 
                codegen("fcmpl\n");
            codegen("ifle L_cmp_%d\n", LabelCount);
            codegen("iconst_0\n");
            codegen("goto L_cmp_%d\n", LabelCount+1);
            INDENT--;
            codegen("L_cmp_%d:\n", LabelCount);
            INDENT++;
            codegen("iconst_1\n");
            INDENT--;
            codegen("L_cmp_%d:\n\n", LabelCount+1);
            INDENT++;
            LabelCount += 2;
        }
;

AndExpr
    : CmpExpr
    | AndExpr AND CmpExpr 
        {   
            printf("AND\n");
            if($1 != "bool" && lookup_symbol($1) != "bool") {
                printf("error:%d: invalid operation: (operator AND not defined on %s)\n", yylineno, $1);
                HAS_ERROR = true;
            }
            else if($3 != "bool" && lookup_symbol($3) != "bool") {
                printf("error:%d: invalid operation: (operator AND not defined on %s)\n", yylineno, $3);
                HAS_ERROR = true;
            }
            codegen("iand\n");
            $$ = "bool"; 
        } 
;

OrExpr
    : AndExpr
    | OrExpr OR AndExpr 
        {
            printf("OR\n");
            if($1 != "bool" && lookup_symbol($1) != "bool")  { 
                printf("error:%d: invalid operation: (operator OR not defined on %s)\n", yylineno, $1);
                HAS_ERROR = true;
            }
            else if($3 != "bool" && lookup_symbol($3) != "bool") { 
                printf("error:%d: invalid operation: (operator OR not defined on %s)\n", yylineno, $3);
                HAS_ERROR = true;
            }
            codegen("ior\n"); 
            $$ = "bool"; 
        }
;

PrimaryExpr 
    : Operand 
    | IndexExpr
    | ConversionExpr 
;

Operand 
    : Literal
    | IDENT 
        { 
            if(lookup_symbol($1) != "-1") 
                store_load($1, 1);    
            else {
                printf("error:%d: undefined: %s\n", yylineno, $1);
                HAS_ERROR = true;
            }
            IS_ARRAY = is_array($1);
            $$ = $1; 
        }
    | LPAREN Expression RPAREN { $$ = $2; }
;

Literal
    : INT_LIT { codegen("ldc %d\n", $<i_val>$); printf("INT_LIT %d\n", $<i_val>$); $$ = "int"; size = $1; }
    | FLOAT_LIT { codegen("ldc %f\n", $<f_val>$); printf("FLOAT_LIT %f\n", $<f_val>$); $$ = "float"; }
    | STRING_LIT { codegen("ldc \"%s\"\n", $<s_val>$); printf("STRING_LIT %s\n", $<s_val>$); $$ = "string"; }
    | TRUE { codegen("iconst_1\n"); $$ = "bool"; printf("TRUE\n"); }
    | FALSE { codegen("iconst_0\n"); $$ = "bool"; printf("FALSE\n"); }
;

IndexExpr 
    : PrimaryExpr LBRACK Expression RBRACK 
        { 
            if(lookup_symbol($1) == "int")
                codegen("iaload\n"); 
            else if(lookup_symbol($1) == "float")
                codegen("faload\n");
        }
;

AssignmentExpr
    : OrExpr
    | UnaryExpr assign_op AssignmentExpr 
        {   
            if($1 != $3 && lookup_symbol($1) != $3 && $1 != lookup_symbol($3) && lookup_symbol($1) != lookup_symbol($3)) { 
                printf("error:%d: invalid operation: ASSIGN (mismatched types %s and %s)\n", yylineno, $1, $3);
                HAS_ERROR = true;
            }
            if($2 != "ASSIGN") {
                store_load($1, 1);
                codegen("swap\n");
                if($2 == "ADD_ASSIGN") {
                    if(lookup_symbol($1) == "float" || $3 == "float" || lookup_symbol($3) == "float")
                        codegen("fadd\n");
                    else 
                        codegen("iadd\n");
                }
                else if($2 == "SUB_ASSIGN") {
                    if(lookup_symbol($1) == "float" || $3 == "float" || lookup_symbol($3) == "float")
                        codegen("fsub\n");
                    else 
                        codegen("isub\n");
                }
                else if($2 == "MUL_ASSIGN") {
                    if(lookup_symbol($1) == "float" || $3 == "float" || lookup_symbol($3) == "float")
                        codegen("fmul\n");
                    else 
                        codegen("imul\n");
                }
                else if($2 == "QUO_ASSIGN") {
                    if(lookup_symbol($1) == "float" || $3 == "float" || lookup_symbol($3) == "float")
                        codegen("fdiv\n");
                    else 
                        codegen("idiv\n");
                }
                else 
                    codegen("irem\n");
            }
            store_load($1, 0);
            printf("%s\n", $2); 
        }
    | Literal assign_op AssignmentExpr 
        {
            printf("error:%d: cannot assign to %s\n", yylineno, $1);
            HAS_ERROR = true;
            printf("%s\n", $2);
        }
;

assign_op 
    : ASSIGN 
        {
            $$ = "ASSIGN"; 
            codegen("pop\n"); 
            if(IS_ARRAY) {
                codegen("aload %d\n", addr); 
                codegen("ldc %d\n", size); 
            }
        }
    | ADD_ASSIGN 
        { 
            $$ = "ADD_ASSIGN"; 
            codegen("pop\n"); 
            if(IS_ARRAY) {
                codegen("aload %d\n", addr); 
                codegen("ldc %d\n", size); 
            }
        }
    | SUB_ASSIGN 
        { 
            $$ = "SUB_ASSIGN"; 
            codegen("pop\n"); 
            if(IS_ARRAY) {
                codegen("aload %d\n", addr); 
                codegen("ldc %d\n", size); 
            }
        }
    | MUL_ASSIGN 
        { 
            $$ = "MUL_ASSIGN";   
            codegen("pop\n"); 
            if(IS_ARRAY) {
                codegen("aload %d\n", addr); 
                codegen("ldc %d\n", size); 
            }
        }
    | QUO_ASSIGN 
        { 
            $$ = "QUO_ASSIGN"; 
            codegen("pop\n"); 
            if(IS_ARRAY) {
                codegen("aload %d\n", addr); 
                codegen("ldc %d\n", size); 
            }
        }
    | REM_ASSIGN {
            $$ = "REM_ASSIGN"; 
            codegen("pop\n"); 
            if(IS_ARRAY) {
                codegen("aload %d\n", addr); 
                codegen("ldc %d\n", size); 
            }
        }
;

StatementList
    : Statement
    | StatementList Statement
;

Statement
    : CompoundStmt
    | DeclarationStmt
    | ExpressionStmt
    | SelectionStmt
    | IterationStmt
    | PrintStmt
;

CompoundStmt
    : LBRACE { scope++; } RBRACE  { dump_symbol(); scope--; }
    | LBRACE { scope++; } BlockList RBRACE { dump_symbol(); scope--; }
;

BlockList
    : BlockItem
    | BlockList BlockItem
;

BlockItem
    : Statement
;

DeclarationStmt
    : Type IDENT SEMICOLON 
        { 
            insert_symbol($2, $1, "-"); 
            if($1 == "int")
                codegen("ldc 0\n");
            else if($1 == "float")
                codegen("ldc 0.0\n");
            else if($1 == "bool")
                codegen("iconst_0\n");
            else 
                codegen("ldc \"\"\n");
            store_load($2, 0); 
        }
    | Type IDENT ASSIGN Expression SEMICOLON 
        { 
            insert_symbol($2, $1, "-"); 
            store_load($2, 0); 
        }
    | Type IDENT LBRACK Expression RBRACK SEMICOLON 
        { 
            insert_symbol($2, "array", $1); 
            codegen("newarray %s\n", $1);
            store_load($2, 2);
        }
;

ExpressionStmt
    : SEMICOLON
    | Expression SEMICOLON
;

SelectionStmt
    : IfStmt 
        {
            INDENT--;
            if(IsExit[scope] == 0)
                codegen("L_if_exit%d:\n", scope);
            IsExit[scope] = 1;
            INDENT++;
        }
    | IfStmt ELSE Statement
        {
            INDENT--;
            if(IsExit[scope] == 0)
                codegen("L_if_exit%d:\n", scope);
            IsExit[scope] = 1;
            INDENT++;
        }
;

IfStmt
    : IF LPAREN Expression RPAREN 
        {   
            codegen("ifeq L_if_false%d_%d\n", scope, IfLabelCount[scope]);
            if($3 != "bool") { 
                printf("error:%d: non-bool (type %s) used as for condition\n", yylineno+1, $3); 
                HAS_ERROR = true;
            }
        }
        Statement {
            codegen("goto L_if_exit%d\n", scope);
            INDENT--;
            codegen("L_if_false%d_%d:\n", scope, IfLabelCount[scope]);
            IfLabelCount[scope]++;
            INDENT++;
        }
;

IterationStmt
    : WHILE 
        {
            INDENT--;
            codegen("L_while_%d_%d:\n", scope, WhileLabelCount[scope]);
            INDENT++;
        }
        LPAREN Expression RPAREN 
        {
            if($4 != "bool")  { 
                printf("error:%d: non-bool (type %s) used as for condition\n", yylineno+1, $4);
                HAS_ERROR = true;
            }
            codegen("ifeq L_while_exit%d_%d\n", scope, WhileLabelCount[scope]);
        }
        Statement 
        {
            codegen("goto L_while_%d_%d\n", scope, WhileLabelCount[scope]);
            INDENT--;
            codegen("L_while_exit%d_%d:\n", scope, WhileLabelCount[scope]);
            INDENT++;
            WhileLabelCount[scope]++;
        }
    | FOR LPAREN ExpressionStmt 
        {
            INDENT--;
            codegen("L_for_condition_%d:\n", ForCount);
            INDENT++;
        }
        ExpressionStmt 
        {
            codegen("ifeq L_for_exit_%d\n", ForCount);
            codegen("goto L_for_body_%d\n", ForCount);
            INDENT--;
            codegen("L_for_post_%d:\n", ForCount);
            INDENT++;
        }
        Expression
        {
            codegen("goto L_for_condition_%d\n", ForCount);
            INDENT--;
            codegen("L_for_body_%d:\n", ForCount);
            INDENT++;
        }
        RPAREN Statement
        {
            codegen("goto L_for_post_%d\n", ForCount);
            INDENT--;
            codegen("L_for_exit_%d:\n", ForCount);
            INDENT++;
            ForCount++;
        }
;



PrintStmt
    : PRINT LPAREN Expression RPAREN SEMICOLON 
        {
            printf("PRINT %s\n", $3); 
            if($3 == "bool" || lookup_symbol($3) == "bool") {
                codegen("ifne L_cmp_%d\n", LabelCount);
                codegen("ldc \"false\"\n");
                codegen("goto L_cmp_%d\n", LabelCount+1);
                INDENT--;
                codegen("L_cmp_%d:\n", LabelCount);
                INDENT++;
                codegen("ldc \"true\"\n");
                INDENT--;
                codegen("L_cmp_%d:\n", LabelCount+1);
                INDENT++;
                LabelCount += 2;
            }
            codegen("getstatic java/lang/System/out Ljava/io/PrintStream;\n");
            codegen("swap\n");
            if($3 == "int" || lookup_symbol($3) == "int")
                codegen("invokevirtual java/io/PrintStream/print(I)V\n\n");
            else if($3 == "float" || lookup_symbol($3) == "float")
                codegen("invokevirtual java/io/PrintStream/print(F)V\n\n");
            else
                codegen("invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n\n");
        }
;

%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }

    /* Codegen output init */
    char *bytecode_filename = "hw3.j";
    fout = fopen(bytecode_filename, "w");
    codegen(".source hw3.j\n");
    codegen(".class public Main\n");
    codegen(".super java/lang/Object\n");
    codegen(".method public static main([Ljava/lang/String;)V\n");
    codegen(".limit stack 100\n");
    codegen(".limit locals 100\n");
    INDENT++;

    yyparse();

	printf("Total lines: %d\n", yylineno);

    /* Codegen end */
    codegen("return\n");
    INDENT--;
    codegen(".end method\n");
    fclose(fout);
    fclose(yyin);

    if (HAS_ERROR) {
        remove(bytecode_filename);
    }
    return 0;
}
