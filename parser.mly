%{ open Ast 

let parse_error s = (* Called by the parser function on error *)
  print_endline s;
  flush stdout

%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA COLON PIPE
%token INT DOUBLE PITCH BOOLEAN SOUND VOID EOF
%token PLUS MINUS TIMES DIVIDE PERCENT NOT NEG
%token OR AND EQ NEQ LT GT LEQ GEQ
%token RETURN IF ELSE FOR WHILE LOOP

%token <string> ID
%token <int> INT_LIT
%token <float> DOUBLE_LIT
%token <bool> BOOLEAN_LIT
/*%token <string> SOUND_LIT*/
%token <string> PITCH_LIT
%token <string> DATATYPE
%token ASSIGN

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left PERCENT
%left NOT NEG

%start program
%type <Ast.program> program

%%

program:
	  /*nothing*/		{[], []}
    | program vdecl 	{ ($2 :: fst $1), snd $1 }
	| program fdecl		{ fst $1, ($2 :: snd $1) }

fdecl:
	DATATYPE ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
	{ { fname = $2;
		rtype = $1;
		formals = $4;
		locals = List.rev $7;
		body = List.rev $8 } }

formals_opt:
	/* nothing */		{ [] }
	| formal_list		{ List.rev $1 }

formal_list:
	param_decl { [$1] }
	| formal_list COMMA param_decl { $3 :: $1 }

param_decl:
	DATATYPE ID
		{ {	paramname = $2;
			paramtype = $1 } }

vdecl_list:   
       			   	     { [] }  
	 | vdecl_list vdecl  { $2 :: $1}   
	
vdecl: 
     DATATYPE ID SEMI { {varname = $2; vartype = $1} }



/*
arr_decl:
	DATATYPE LBRACK RBRACK ID SEMI
	{ { arrtype = $1;
		arrname = $4 } }
*/

stmt_list:
	/* nothing */ { [] }
	| stmt_list stmt { $2 :: $1 }
	
stmt:
	  expr SEMI {Expr($1) }
	| RETURN expr SEMI { Return($2) }
	| LBRACE stmt_list RBRACE { Block(List.rev $2) }
	| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
	| IF LPAREN expr RPAREN stmt ELSE stmt	  { If($3, $5, $7) }
	| FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt
    	{ For($3, $5, $7, $9) }
	| WHILE LPAREN expr RPAREN stmt			  { While($3, $5) }
	| LOOP LPAREN ID COLON ID RPAREN stmt	  { Loop($3, $5, $7) }
	
expr:
	  INT_LIT					  { Int($1) }
  | DOUBLE_LIT                    { Double($1) }
  | BOOLEAN_LIT                   { Boolean($1) }
  | PIPE pitch_list PIPE COLON DOUBLE_LIT COLON INT_LIT 	  { Sound($2, $5, $7) }
  | PITCH_LIT                     { Pitch($1) }
  | ID index_opt				  { Index($1, $2) }
  | ID								{ Id($1) }
  | ID LPAREN actuals_opt RPAREN 	{ Call($1, $3) }
  | LBRACK actuals_opt RBRACK           { Array($2) }
  | expr ASSIGN expr 					{ Assign($1, $3) }
  | expr PLUS expr						{ Binop($1, Add, $3) }
	| expr MINUS	expr				{ Binop($1, Sub, $3) }
	| expr TIMES	expr				{ Binop($1, Mult, $3) }
	| expr DIVIDE expr					{ Binop($1, Div, $3) }
	| expr PERCENT expr					{ Binop($1, Mod, $3) }
	| NOT expr							{ Not($2) }
	| MINUS expr						{ Neg($2) }
	| expr OR expr						{ Binop($1, Or, $3) }
	| expr AND expr						{ Binop($1, And, $3) }
	| expr EQ expr						{ Binop($1, Eq, $3) }
	| expr NEQ expr						{ Binop($1, Neq, $3) }
	| expr LT expr						{ Binop($1, Lt, $3) }
	| expr GT expr						{ Binop($1, Gt, $3) }
	| expr LEQ expr						{ Binop($1, Leq, $3) }
	| expr GEQ expr						{ Binop($1, Geq, $3) }
	| LPAREN expr RPAREN				{ $2 }
	

pitch_list:
	 PITCH_LIT { [$1] }
	| pitch_list COMMA PITCH_LIT { $3 :: $1 }


actuals_opt:
	/*nothing*/ 	{ [] }
	| actuals_list	{List.rev $1}

actuals_list:
	  expr						{ [$1] }
	| actuals_list COMMA expr 	{ $3 :: $1 }

index_opt:
	indices { List.rev $1 }
	
indices:
	  LBRACK expr RBRACK { [$2] }
	| indices LBRACK expr RBRACK { $3 :: $1 }
