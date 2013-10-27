%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK SEMI COMMA PLUS MINUS TIMES DIVIDE MOD
%token ASSIGN EQ NEQ LT LEQ GT GEQ NOT AND OR
%token IF ELSE FOR WHILE RETURN INT PITCH SOUND VOID EOF
%token <int> LITERAL
%token <string> ID

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left AND OR
%left LT GT LEQ GEQ EQ NEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD

%start program
%type <Ast.program> program

%%

program:
    /* nothing */   { [], [] }
    | program vdecl { ($2 :: fst $1), snd $1 }
    | program fdecl { fst $1, ($2 :: snd $1) }

vdecl_list:
    /* nothing */         { [] }
    | vdecl_list vdecl    { $2 :: $1  }
    
vdecl:
    typeConst ID SEMI			
            { { vartype = $1;
                varname = $2 } } 

fdecl:
	typeConst ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
                    { { rettype = $1;
                        fname = $2;
						formals = $4;
						locals = List.rev $7;
						body = List.rev $8 } }

formals_opt:
    /* nothing */           { [] }
    | formal_list           { List.rev $1 }

formal_list:
    formal_decl                         { [$1] }
    | formal_list COMMA formal_decl     { $3 :: $1 }

formal_decl:
     typeConst ID
             { { formname = $2;
                 formtype = $1; } }          
                    
typeConst:
    INT                     { Int }
    | VOID                  { Void }
    | PITCH                 { Pitch }
    | SOUND                 { Sound }

stmt_list:
    /* nothing */             { [] }
	| stmt_list stmt          { $2 :: $1 }

stmt:
    expr SEMI                       { Expr($1) } 
    | RETURN expr SEMI              { Return($2) }
    | LBRACE stmt_list RBRACE       { Block(List.rev $2) }
    | IF LPAREN expr RPAREN stmt    { If($3, $5, Block([])) }
    /*elseif*/
    | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
    | ID LPAREN actuals_opt RPAREN  { Call($1, $3) }

actuals_opt: 
    /* nothing */   { [] }
    | actuals_list  {List.rev $1}

actuals_list:
    expr                        { [$1] }
    | actuals_list COMMA expr   { $3 :: $1 } 

expr:
	ID									{ Id($1) }
	| LPAREN MINUS expr	RPAREN			{ Neg($3) } 
	| NOT LPAREN expr RPAREN			{ Not($3) }  
	| expr PLUS expr					{ Binop($1, Plus, $3) }
	| expr MINUS expr					{ Binop($1, Minu, $3) }
	| expr TIMES expr					{ Binop($1, Mult, $3) }
	| expr DIVIDE expr					{ Binop($1, Divi, $3) }
	| expr MOD expr						{ Binop($1, Mod , $3) }
	| expr LT expr						{ Binop($1, Lt  , $3) }
	| expr GT expr						{ Binop($1, Gt  , $3) }
	| expr LEQ expr						{ Binop($1, Leq , $3) }
	| expr GEQ expr						{ Binop($1, Geq , $3) }
	| expr EQ expr						{ Binop($1, Eq  , $3) }
	| expr AND expr						{ Binop($1, And , $3) }
	| expr OR expr						{ Binop($1, Or  , $3) }
	| expr ASSIGN expr					{ Binop($1, Asn , $3) }
	| LPAREN expr RPAREN				{ Expr($2) }
	| ID LPAREN actuals_opt RPAREN		{ Call($1, $3) }
	| ID LBRACK expr RBRACK				{ Array($1, $3) }
	
