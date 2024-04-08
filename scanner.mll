(*Scanner*)

{ open Parser }

(*Letters and Digits*)
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
    [' ' '\t' '\r' '\n']    { token lexbuf }
  | "/*"                    { comment lexbuf }
  | ".trml"                 { TRML }
  | "Root"                  { ROOT }
  | "Node"                  { NODE }
  | "->"                    { RARROW }
  | "`"                     { BTICK }
  | '('                     { LPAREN }
  | ')'                     { RPAREN }
  | '['                     { LBRACK }
  | ']'                     { RBRACK }
  | '{'                     { LBRACE }
  | '}'                     { RBRACE }
  | ';'                     { SEMI }
  | ','                     { COMMA }
  | '+'                     { PLUS }
  | '-'                     { MINUS }
  | '*'                     { TIMES }
  | '/'                     { DIVIDE }
  | '%'                     { MOD }
  | '='                     { ASSIGN }
  | "=="                    { EQ }
  | "!="                    { NEQ }
  | "<="                    { LEQ }
  | '<'                     { LT }
  | ">="                    { GEQ }
  | '>'                     { GT }
  | "and"                   { AND }
  | "or"                    { OR }
  | "not"                   { NOT }
  | "if"                    { IF }
  | "ifne"                  { IFNOELSE }
  | "else"                  { ELSE }
  | "while"                 { WHILE }
  | "for"                   { FOR }
  | "in"                    { IN }
  | ":"                     { COLON }
  | "return"                { RETURN }
  | "int"                   { INT }
  | "bool"                  { BOOL }
  | "true"                  { BLIT(true)  }
  | "false"                 { BLIT(false) }
  (*trs forward/backward connectors*)
  | "forward"               { FORWARD }
  | "give"                  { GIVE }
  | "create"                { CREATE }
  | "backward"              { BACKWARD }
  | "combine"               { COMBINE }
  (*fuck camltree running functions*)
  | "check"    	            { CHECK }
  | "run"                   { RUN }
  | "replace"               { REPLACE }
  
  | digit+ as lem           { LITERAL(int_of_string lem) }
  | letter (digit | letter | '_')* as lem    { ID(lem) }
  | eof                     { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
    "*/" { token lexbuf }
  | _    { comment lexbuf }

