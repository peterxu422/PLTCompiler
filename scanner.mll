{ open Parser }

let int_lit = ['0'-'9']+

(*
let pitch = (['A' - 'G']('#' | 'b')?['0' - '9'] | ['C' - 'G']('#' |'b')?"10")
let dbl_lit = ['0'-'9']+['.']['0' - '9']+
let int_over_int = int_lit['/']int_lit
let comma_pitch = pitch(','| ", ")
let comma_id = id(','| ", ")
let pitches = pitch | comma_pitch*pitch
let ids = id | comma_id*id
let array_of_pitches = ['[']pitches[']']
let array_of_ids = ['[']ids[']']
let sound = (array_of_pitches | id | array_of_ids)[':'](id | int_over_int)[':'](id | int_lit)
*)
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
[' ' '\t' '\r' '\n']    	{token lexbuf}
| "/*"                      {comment lexbuf}
| "//"						{comment_newline lexbuf}
| '('       { LPAREN }          
| ')'       { RPAREN }
| '{'       { LBRACE }          
| '}'       { RBRACE }
| ';'       { SEMI }
| ','		{ COMMA }
| "int" { DATATYPE("int") }
| id      as lxm { ID(lxm) }
| int_lit as lxm { INT_LIT(int_of_string lxm) }
| eof           { EOF }

(*| '['       { LBRACK }          
| ']'       { RBRACK }
| ':'       { COLON }
| ','       { COMMA }           
| '"'       { QUOTE }
| '+'       { PLUS }            
| '-'       { MINUS }
| '*'       { TIMES }           
| '/'       { DIVIDE }
| '%'	    { PERCENT }			
| '!'       { NOT }
| '='       { ASSIGN }          
| '^'       { TIE }
| "||"	    { OR }				
| "&&"	    { AND }
| "=="      { EQ }				
| "!="      { NEQ } 
| '<'       { LT }
| '>'       { GT }
| "<="      { LEQ }             
| ">="          { GEQ }             
| "if"      { IF }  
| "else"        { ELSE }            
| "for"     { FOR }
| "while"       { WHILE }           
| "return"  { RETURN }
| "int"         { INT }             
| "double"  { DBL }
| "true"        { TRUE }            
| "false"   { FALSE }
| "function"    { FUNC }            
| "pitch"   { PITCH }           
| "sound"       { SOUND }           
| "void"    { VOID }
| "main"        { MAIN }      	   	
| sound   as lxm { S_LIT(lxm) }
| pitch   as lxm { P_LIT(lxm) }
| dbl_lit as lxm { DBL_LIT(float_of_string lxm)}

| ['A' - 'G']['#' 'b']?['0' - '9']  as lxm { PLITERAL(lxm) }
| ['0'-'9']+ as lxm { INT_LIT(int_of_string lxm) }
*)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }


and comment = parse
"*/"    { token lexbuf }
| _       { comment lexbuf }


and comment_newline = parse
'\n'	{token lexbuf}
| _ {comment_newline lexbuf}
