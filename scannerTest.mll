{ type token  =  LPAREN | LBRACE | LBRACK | SEMI | COMMA | 
	PLUS | TIMES | PERCENT | ASSIGN | RPAREN | RBRACE | RBRACK | 
	COLON | QUOTE | MINUS | DIVIDE | NOT | TIE | AND | NEQ | LT | 
	GT | IF | FOR | RETURN | DBL | FALSE | PITCH | VOID | OR | EQ |
	 COMMENT | LEQ | GEQ | ELSE | WHILE | INT | TRUE | FUNC | SOUND |
	 MAIN | EOF | S_LIT of string  | P_LIT of string  | INT_LIT of int | ID of string | DBL_LIT of float
} 

let pitch = (['A' - 'G']('#' | 'b')?['0' - '9'] | ['C' - 'G']('#' |'b')?"10")
let int_lit = ['0'-'9']+
let dbl_lit = ['0'-'9']+['.']['0' - '9']+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int_over_int = int_lit['/']int_lit
let comma_pitch = pitch(','| ", ")
let comma_id = id(','| ", ")
let pitches = pitch | comma_pitch*pitch
let ids = id | comma_id*id
let array_of_pitches = ['[']pitches[']']
let array_of_ids = ['[']ids[']']
let sound = (array_of_pitches | id | array_of_ids)[':'](id | int_over_int)[':'](id | int_lit)

rule token = parse
[' ' '\t' '\r' '\n']    	{token lexbuf}
| "/*"                      {comment lexbuf}
| "//"						{comment_newline lexbuf}
| '('       { LPAREN }          | ')'       { RPAREN }
| '{'       { LBRACE }          | '}'       { RBRACE }
| '['       { LBRACK }          | ']'       { RBRACK }
| ';'       { SEMI }            | ':'       { COLON }
| ','       { COMMA }           | '"'       { QUOTE }
| '+'       { PLUS }            | '-'       { MINUS }
| '*'       { TIMES }           | '/'       { DIVIDE }
| '%'	    { PERCENT }			| '!'       { NOT }
| '='       { ASSIGN }          | '^'       { TIE }
| "||"	    { OR }				| "&&"	    { AND }
| "=="      { EQ }				| "!="      { NEQ } 
| '<'       { LT }
| "<="      { LEQ }             | '>'       { GT }
| ">="          { GEQ }             | "if"      { IF }  
| "else"        { ELSE }            | "for"     { FOR }
| "while"       { WHILE }           | "return"  { RETURN }
| "int"         { INT }             | "double"  { DBL }
| "true"        { TRUE }            | "false"   { FALSE }
| "function"    { FUNC }            | "pitch"   { PITCH }           
| "sound"       { SOUND }           | "void"    { VOID }
| "main"        { MAIN }      	   	
| eof           { EOF }
| sound   as lxm { S_LIT(lxm) }
| pitch   as lxm { P_LIT(lxm) }
| int_lit as lxm { INT_LIT(int_of_string lxm) }
| dbl_lit as lxm { DBL_LIT(float_of_string lxm)}
| id      as lxm { ID(lxm) }

and comment = parse
"*/"    { token lexbuf }
| _       { comment lexbuf }

and comment_newline = parse
'\n'	{token lexbuf}
| _ {comment_newline lexbuf}

{
(*TESTING*)
let lexbuf = Lexing.from_channel stdin in
let tester = 
	let rec next l =
		match token lexbuf with
		EOF -> l
		| LPAREN -> next("LPAREN" :: l)
		| RPAREN -> next("RPAREN" :: l)
		| ELSE -> next("ELSE" :: l)
		| NEQ -> next("NEQ" :: l)
		| FUNC -> next("FUNC" :: l)
		| VOID -> next("VOID" :: l)
		| EQ -> next("EQ" :: l)
		| LBRACE -> next("LBRACE" :: l)
		| RBRACE -> next("RBRACE" :: l)
		| SEMI -> next("SEMI" :: l)
		| SOUND -> next("SOUND" :: l)
		| ASSIGN -> next("ASSIGN" :: l)
		| PITCH -> next("PITCH" :: l)
		| MAIN -> next("MAIN" :: l)
		| TRUE -> next("TRUE" :: l)
		| INT -> next("INT" :: l)
		| WHILE -> next("WHILE" :: l)
		| GEQ -> next("GEQ" :: l)
		| LEQ -> next("LEQ" :: l)
		| COMMENT -> next("COMMENT" :: l)
		| FALSE -> next("FALSE" :: l)
		| OR -> next("OR" :: l)
		| DBL -> next("DBL" :: l)
		| RETURN -> next("RETURN" :: l)
		| FOR -> next("FOR" :: l)
		| IF -> next("IF" :: l)
		| GT -> next("GT" :: l)
		| LT -> next("LT" :: l)
		| AND -> next("AND" :: l)
		| TIE -> next("TIE" :: l)
		| NOT -> next("NOT" :: l)
		| DIVIDE -> next("DIVIDE" :: l)
		| MINUS -> next("MINUS" :: l)
		| QUOTE -> next("QUOTE" :: l)
		| COLON -> next("COLON" :: l)
		| RBRACK -> next("RBRACK" :: l)
		| PERCENT -> next("PERCENT" :: l)
		| TIMES -> next("TIMES" :: l)
		| PLUS -> next("PLUS" :: l)
		| COMMA -> next("COMMA" :: l)
		| LBRACK -> next("LBRACK" :: l)
		| ID(lit) -> next("ID" :: l)
		| INT_LIT(lit) -> next("INT_LIT" :: l)
		| DBL_LIT(lit) -> next("DBL_LIT" :: l)
		| P_LIT(lit) -> next("P_LIT" :: l)
		| S_LIT(lit) -> next("S_LIT" :: l)
	in next []
in

let out_list = ["RBRACE"; "SEMI"; "DBL_LIT"; "ASSIGN"; "ID"; "DBL"; "SEMI"; "S_LIT"; "ASSIGN"; "ID"; "SOUND"; "SEMI"; "S_LIT"; "ASSIGN"; "ID"; "SOUND"; "SEMI"; "S_LIT"; "ASSIGN"; "ID"; "SOUND"; "NEQ"; "SEMI"; "P_LIT"; "ASSIGN"; "ID"; "PITCH"; "LBRACE"; "RPAREN"; "LPAREN"; "ID"; "VOID"; "FUNC"] in



let rec eq_list out test = match out, test with
	[], [] -> true
	| h1::t1, h2::t2 -> if h1 = h2 then eq_list t1 t2 else false
	| _, _ -> false
in

if eq_list out_list tester
then 
	let output = "Scanner is parsing tokens correctly" in
		print_endline output 
else 
	let output = "Tokens and expected output differed" in
		print_endline output;
		print_endline "parsed tokens:";
		List.iter (fun s -> print_endline s) tester;
		print_endline "expected tokens:";
		List.iter (fun s -> print_endline s) out_list
}