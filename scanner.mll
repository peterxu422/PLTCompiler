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
| '='       { ASSIGN }          | '^'       { TIE }
| '!'       { NOT }             | "=="      { EQ }
| "!="      { NEQ }             | '<'       { LT }
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
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

and comment = parse
  "*/"    { token lexbuf }
| _       { comment lexbuf }

      
