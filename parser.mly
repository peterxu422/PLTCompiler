%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token LBRACK RBRACK
%token PLUS MINUS TIMES DIVIDE PERCENT NOT CARROT
%token OR AND EQ NEQ LT GT LEQ GEQ
%token INT DOUBLE PITCH BOOLEAN SOUND VOID EOF 

%token <string> ID
%token <int> INT_LIT
%token <float> DOUBLE_LIT
%token <bool> BOOLEAN_LIT
%token <string> SOUND_LIT
%token <string> PITCH_LIT
%token <string> DATATYPE
%token ASSIGN

%right ASSIGN
%left	OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left PERCENT
%left NOT
%left CARROT

%type <Ast.program> program
%start program

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
	
stmt_list:
	/* nothing */ { [] }
	| stmt_list stmt { $2 :: $1 }
	
stmt:
	  expr SEMI {Expr($1) }
	| LBRACE stmt_list RBRACE { Block(List.rev $2) }
	
expr:
	  INT_LIT												{ Int($1) }
  | DOUBLE_LIT                    { Double($1) }
  | BOOLEAN_LIT                   { Boolean($1) }
  | PITCH_LIT                     { Pitch($1) }
	| SOUND_LIT											{ Sound($1) }
	| ID														{ Id($1) }
	| ID LPAREN actuals_opt RPAREN 	{ Call($1, $3) }
  | LBRACK array RBRACK           { Array(List.rev $2) }
  | LBRACK RBRACK                 { Array([]) }
  | expr ASSIGN expr 							{ Assign($1, $3) }
  | expr PLUS expr								{ Binop($1, Add, $3) }
	| expr MINUS	expr							{ Binop($1, Sub, $3) }
	| expr TIMES	expr							{ Binop($1, Mult, $3) }
	| expr DIVIDE expr							{ Binop($1, Div, $3) }
	| expr PERCENT expr							{ Binop($1, Mod, $3) }
	| NOT expr											{ Neg($2) }
	| expr CARROT										{ Tie($1) }
	| expr OR expr									{ Binop($1, Or, $3) }
	| expr AND expr									{ Binop($1, And, $3) }
	| expr EQ expr									{ Binop($1, Eq, $3) }
	| expr NEQ expr									{ Binop($1, Neq, $3) }
	| expr LT expr									{ Binop($1, Lt, $3) }
	| expr GT expr									{ Binop($1, Gt, $3) }
	| expr LEQ expr									{ Binop($1, Leq, $3) }
	| expr GEQ expr									{ Binop($1, Geq, $3) }


array: 
     expr { [$1] }
    | array COMMA expr { $3 :: $1 }
	
actuals_opt:
	/*nothing*/ 	{ [] }
	| actuals_list	{List.rev $1}

actuals_list:
	  expr						{ [$1] }
	| actuals_list COMMA expr 	{ $3 :: $1 }