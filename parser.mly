%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token INT DOUBLE PITCH BOOLEAN SOUND VOID EOF

%token <int> INT_LIT
%token <float> DOUBLE_LIT
%token <bool> BOOLEAN_LIT
%token <string> ID
%token <string> DATATYPE


%type <Ast.program> program
%start program

%%

program:
	  /*nothing*/		{[], []}
    | program vdecl { ($2 :: fst $1), snd $1 }
	| program fdecl		{ fst $1, ($2 :: snd $1) }

fdecl:
	ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
	{ { fname = $1;
		formals = $3;
		body = List.rev $6 } }

formals_opt:
	/* nothing */		{ [] }
	| formal_list		{ List.rev $1 }

formal_list:
	  ID					{ [$1] }
	| formal_list COMMA ID	{$3 :: $1}

/* vdecl_list:   */
      /* nothing */	   /*  { [] } */ 
	/* | vdecl_list vdecl  { $2 :: $1}  */ 
	
vdecl: 
    DATATYPE ID SEMI { {varname = $2; vartype = $1} }
	
stmt_list:
	/* nothing */ { [] }
	| stmt_list stmt { $2 :: $1 }
	
stmt:
	  expr SEMI {Expr($1) }
	| LBRACE stmt_list RBRACE { Block(List.rev $2) }
	
expr:
	  INT_LIT						{ Int($1) }
    | DOUBLE_LIT                    { Double($1) }
    | BOOLEAN_LIT                   { Boolean($1) }
	| ID							{ Id($1) }
	| ID LPAREN actuals_opt RPAREN 	{ Call($1, $3) }
	
actuals_opt:
	/*nothing*/ 	{ [] }
	| actuals_list	{List.rev $1}

actuals_list:
	  expr						{ [$1] }
	| actuals_list COMMA expr 	{ $3 :: $1 }