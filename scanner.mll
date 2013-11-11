{ open Parser }

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
| ['A' - 'G']['#' 'b']?['0' - '9']  as lxm { PLITERAL(lxm) }
| ['0'-'9']+ as lxm { INTLITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

and comment = parse
  "*/"    { token lexbuf }
| _       { comment lexbuf }

      
