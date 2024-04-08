(*Scanner*)

{ open Parser }

(*Letters and Digits*)
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
    [' ' '\t' '\r' '\n']    { token lexbuf }
  | "/*"                    { comment lexbuf }
  
  (*Syntactic Groupers*)
  | '('                     { LPAREN }
  | ')'                     { RPAREN }
  | '['                     { LBRACK }
  | ']'                     { RBRACK }
  | '{'                     { LBRACE }
  | '}'                     { RBRACE }
  | ';'                     { SEMI }
  | ','                     { COMMA }
  
  (*Operators*)
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
  
  (*Control Flow*)
  | "if"                    { IF }
  | "ifne"                  { IFNOELSE }
  | "else"                  { ELSE }
  | "while"                 { WHILE }
  | "for"                   { FOR }
  | "in"                    { IN }
  | ":"                     { COLON }
  | "return"                { RETURN }
  
  (*Forward/Backward Evaluation*)
  | "forward"               { FORWARD }
  | "create"                { CREATE }
  | "give"                  { GIVE }
  | "backward"              { BACKWARD }
  | "eval"                  { EVAL }
  
  (*Oasis TRS Runtime*)
  | "check"                 { CHECK }
  | "run"                   { RUN }
  | "replace"               { REPLACE }
  
  (*Types*)
  | "int"                   { INT }
  | "bool"                  { BOOL }
  | ".trml"                 { TRML }
  | "Root"                  { ROOT }
  | "Node"                  { NODE }
  
  (*Oasis Symbols*)
  | "<-"                    { LARROW }
  | "->"                    { RARROW }
  | "`"                     { BTICK }
  | "@"                     { AT }
  
  (*Literals and Strings*)
  | "True"                  { BLIT(true) }
  | "False"                 { BLIT(false) }
  | digit+ as lem           { LITERAL(int_of_string lem) }
  | letter (digit | letter | '_')* as lem    { ID(lem) }
  | eof                     { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
    "*/" { token lexbuf }
  | _    { comment lexbuf }
