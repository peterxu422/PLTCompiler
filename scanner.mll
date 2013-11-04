{ open Parser }

let pitch = (['A' - 'G']('#' | 'b')?['0' - '9'] | ['C' - 'G']('#' |'b')?"10")
let int_lit = ['0'-'9']+
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
| sound   as lxm { SLIT(lxm) }
| pitch   as lxm { PLIT(lxm) }
| int_lit as lxm { LITERAL(int_of_string lxm) }
| id      as lxm { ID(lxm) }

and comment = parse
"*/"    { token lexbuf }
| _       { comment lexbuf }

and comment_newline = parse
'\n'	{token lexbuf}
| _ {comment_newline lexbuf}