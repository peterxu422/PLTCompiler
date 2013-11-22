%{ open Ast 

let parse_error s = (* Called by the parser function on error *)
  print_endline s;
  flush stdout

%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA
%token INT DOUBLE PITCH BOOLEAN SOUND VOID EOF
%token RETURN IF ELSE

%token <string> ID
%token <int> INT_LIT
%token <float> DOUBLE_LIT
%token <bool> BOOLEAN_LIT
%token <string> SOUND_LIT
%token <string> PITCH_LIT
%token <string> DATATYPE
%token ASSIGN

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN

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
	
stmt_list:
	/* nothing */ { [] }
	| stmt_list stmt { $2 :: $1 }
	
stmt:
	  expr SEMI {Expr($1) }
	| RETURN expr SEMI { Return($2) }
	| LBRACE stmt_list RBRACE { Block(List.rev $2) }
	| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
	| IF LPAREN expr RPAREN stmt ELSE stmt	  { If($3, $5, $7) }
	
expr:
	  INT_LIT						{ Int($1) }
    | DOUBLE_LIT                    { Double($1) }
    | BOOLEAN_LIT                   { Boolean($1) }
    | PITCH_LIT                     { Pitch($1) }
	| SOUND_LIT						{ Sound($1) }
	| ID							{ Id($1) }
	| ID LPAREN actuals_opt RPAREN 	{ Call($1, $3) }
    | LBRACK array RBRACK           { Array(List.rev $2) }
    | LBRACK RBRACK                 { Array([]) }
    | expr ASSIGN expr 				{ Assign($1, $3) }

array: 
     expr { [$1] }
    | array COMMA expr { $3 :: $1 }
	
actuals_opt:
	/*nothing*/ 	{ [] }
	| actuals_list	{List.rev $1}

actuals_list:
	  expr						{ [$1] }
	| actuals_list COMMA expr 	{ $3 :: $1 }