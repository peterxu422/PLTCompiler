{ open Parser }

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
| '('       { LPAREN }          
| ')'       { RPAREN }
| '{'       { LBRACE }          
| '}'       { RBRACE }
| ';'       { SEMI }
| ','		{ COMMA }
| '['       { LBRACK }          
| ']'       { RBRACK }
| '=' 		{ ASSIGN }
| "int"     { DATATYPE("int") }
| "double"  { DATATYPE("double") }
| "boolean" { DATATYPE("boolean") }
| "pitch"   { DATATYPE("pitch") }
| "sound"   { DATATYPE("sound") } 
| "void"    { DATATYPE("void") }
| "true"|"false" as lxm { BOOLEAN_LIT(bool_of_string lxm)}
| "return"  { RETURN }
| "if"		{ IF }
| "else"	{ ELSE }
| id             as lxm { ID(lxm) }
| int_lit        as lxm { INT_LIT(int_of_string lxm) }
| dbl_lit        as lxm { DOUBLE_LIT(float_of_string lxm)}
| pitch          as lxm { PITCH_LIT(lxm)}
| sound   		 as lxm { SOUND_LIT(lxm) }
| eof       { EOF }

(*| ':'       { COLON }
| ','       { COMMA }           
| '"'       { QUOTE }
| '+'       { PLUS }            
| '-'       { MINUS }
| '*'       { TIMES }           
| '/'       { DIVIDE }
| '%'	    { PERCENT }			
| '!'       { NOT }
| '^'       { TIE }
| "||"	    { OR }				
| "&&"	    { AND }
| "=="      { EQ }				
| "!="      { NEQ } 
| '<'       { LT }
| '>'       { GT }
| "<="      { LEQ }             
| ">="          { GEQ }             
  
            
| "for"     { FOR }
| "while"       { WHILE }           

| "function"    { FUNC }            
| "main"        { MAIN }      	   	


//The following below are either unnecessary or redundant
| "int"         { INT }             
| "double"  { DBL }
| "pitch"   { PITCH }                     
| "void"    { VOID }
| "true"        { TRUE }            
| "false"   { FALSE }

| pitch   as lxm { P_LIT(lxm) }
| dbl_lit as lxm { DBL_LIT(float_of_string lxm)}
| ['A' - 'G']['#' 'b']?['0' - '9']  as lxm { PLITERAL(lxm) }
| ['0'-'9']+ as lxm { INT_LIT(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
*)

and comment = parse
"*/"    { token lexbuf }
| _       { comment lexbuf }


and comment_newline = parse
'\n'	{token lexbuf}
| _ {comment_newline lexbuf}
