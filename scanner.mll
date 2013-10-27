{ open Parser }

let pitch = (['A' - 'G']('#' | 'b')?['0' - '9'] | ['C' - 'G']('#' |'b')?"10")
let int_lit = ['0'-'9']+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int_over_int = int_lit['/']int_lit
let comma_pitch = pitch[',']
let pitches = pitch | comma_pitch*pitch
let array_of_pitches = ['[']pitches[']']
let sound = (array_of_pitches | id)[':'](id | int_over_int)[':'](id | int_lit)

rule token = parse
    [' ' '\t' '\r' '\n']    {token lexbuf}
| "/*"                      {comment lexbuf}
| '('       { LPAREN }          | ')'       { RPAREN }      (* symbols *)
| '{'       { LBRACE }          | '}'       { RBRACE }
| '['       { LBRACK }          | ']'       { RBRACK }
| ';'       { SEMI }            | ':'       { COLON }
| ','       { COMMA }           | '"'       { QUOTE }
| '+'       { PLUS }            | '-'       { MINUS }
| '*'       { TIMES }           | '/'       { DIVIDE }
| '%'	    { PERCENT }			| '!'       { NOT }
| '='       { ASSIGN }          | '^'       { TIE }
| "||"	   { OR }				| "&&"	   { AND }
| "=="      { EQ }				| "!="      { NEQ } 
| "//"		{ COMMENT }			| '<'       { LT }
| "<="      { LEQ }             | '>'       { GT }
| ">="          { GEQ }             | "if"      { IF }      (* keywords *)
| "else"        { ELSE }            | "for"     { FOR }
| "while"       { WHILE }           | "return"  { RETURN }
| "int"         { INT }             | "double"  { DBL }
| "true"        { TRUE }            | "false"   { FALSE }
| "function"    { FUNC }            | "pitch"   { PITCH }           
| "sound"       { SOUND }           | "void"    { VOID }
| "main"        { MAIN }           	
| eof           { EOF }
| sound   as lxm { SOUND(lxm) }
| pitch   as lxm { PLIT(lxm) }
| int_lit as lxm { LITERAL(int_of_string lxm) }
| id as lxm { ID(lxm) }

and comment = parse
  "*/"    { token lexbuf }
| _       { comment lexbuf }