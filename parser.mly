/*Parser*/

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACK, RBRACK, LBRACE RBRACE PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ AND OR NOT NEG
%token IF ELSE WHILE FOR IN COLON INT BOOL
%token TRML ROOT NODE RARROW BTICK NONE
%token RETURN COMMA
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT

%%

/*Program - TRML definitions portion and program main*/
/*TODO: separate definitions and tscript into two files*/
program:
  trml tscript EOF { let (tn, tl, tv)=$1 in let (dv, df)=$2 in (tn, tl, tv, dv, df) }

/*TRML - define node types, define nodes, initialize nodes*/
trml:
  TRML LBRACE trmldecls RBRACE { $3 }
trmldecls:
   { ([], [], []) }
 | nodedecl SEMI trmldecls { let (nd,ld,vd) = $3 in (($1 :: nd), ld, vd) }
 | linkdecl SEMI trmldecls { let (nd,ld,vd) = $3 in (nd, ($1 :: ld), vd) }
 | valsdecl SEMI trmldecls { let (nd,ld,vd) = $3 in (nd, ld, ($1 :: vd)) }

/*Node Type Types*/
ntyp:
    ROOT    { Root }
  | NODE    { Node }

/*Node Information*/
/*node type declaration, node declaration, node initialization*/
nodedecl:
   ntyp ID                                 { ($1, $2) }
linkdecl:
   ID ID RARROW ID                        { ($1, $2, StringOpt($4)) }
 | ID ID                                { ($1, $2, None) }
valsdecl:
   ID ASSIGN typ expr BTICK typ expr    { ($1, ValOpt($3, $4), ValOpt($6, $7)) }
 | ID ASSIGN typ expr                    { ($1, ValOpt($3, $4), None) }
 | ID ASSIGN BTICK typ expr             { ($1, None, ValOpt($4, $5)) }

/*Tree Script File*, should give 4 things: 
variable, function, forward, backward declaration /
tscript:
   { ([], [], [], []) }
 | vdecl SEMI tscript    { 
   let (cur_vdecl, cur_fdecl, cur_fwddecl, cur_bckdecl) = $3 in 
   (($1::cur_vdecl), cur_fdecl, cur_fwddecl, cur_bckdecl)
  }
 | fdecl tscript    { 
   let (cur_vdecl, cur_fdecl, cur_fwddecl, cur_bckdecl) = $3 in 
   (cur_vdecl, ($1::cur_fdecl), cur_fwddecl, cur_bckdecl)
  }
 | fwddecl tscript   { 
   let (cur_vdecl, cur_fdecl, cur_fwddecl, cur_bckdecl) = $3 in 
   (cur_vdecl, cur_fdecl, ($1::cur_fwddecl), cur_bckdecl)
  }
 | bckdecl tscript      { 
   let (cur_vdecl, cur_fdecl, cur_fwddecl, cur_bckdecl) = $3 in 
   (cur_vdecl, cur_fdecl, cur_fwddecl, ($1::cur_bckdecl))
  }

vdecl_list:
    { [] }
  | vdecl SEMI vdecl_list      { $1 :: $3 }

vdecl:
  typ ID    { ($1, $2) }

typ:
    INT        { Int }
  | BOOL    { Bool }

fdecl:
  vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals=$3;
      locals=$6;
      body=$7
    }
  }

fwddecl:
  FORWARD ID RARROW ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE 
 {
    {
      pnode = $2;
      cnode = $4;
      formals = $6; 
      locals = $9;
      body = $10;
    }
 }

formals_opt:
    { [] }
  | formals_list { $1 }

formals_list:
    vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
    { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr SEMI                                   { Expr $1 }
  | LBRACE stmt_list RBRACE                     { Block $2 }
  | IF LPAREN expr RPAREN stmt ELSE stmt        { If ($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt               { While ($3, $5) }
  | FOR LPAREN ID IN LBRACK expr_list RBRACK RPAREN stmt    { For ($3, $6, $9) }
  | RETURN expr SEMI                            { Return $2 }

expr:
    LITERAL             { Literal($1) }
  | BLIT                { BoolLit($1) }
  | ID                  { Id($1) }
  | expr PLUS expr      { Binop($1, Add, $3) }
  | expr MINUS expr     { Binop($1, Sub, $3) }
  | expr TIMES expr     { Binop($1, Tim, $3) }
  | expr DIVIDE expr    { Binop($1, Div, $3) }
  | expr EQ expr        { Binop($1, Equal, $3) }
  | expr NEQ expr       { Binop($1, Neq, $3) }
  | expr LT expr        { Binop($1, Less, $3) }
  | expr LEQ expr       { Binop($1, Leq, $3) }
  | expr GT expr        { Binop($1, Great, $3) }
  | expr GEQ expr       { Binop($1, Geq, $3) }
  | expr AND expr       { Binop($1, And, $3) }
  | expr OR expr        { Binop($1, Or, $3) }
  | NOT expr            { Unop(Not, $2) }
  | ID ASSIGN expr      { Assign($1, $3) }
  | LPAREN expr RPAREN  { $2 }
  | ID LPAREN args_opt RPAREN    { Call ($1, $3) }

expr_list:
    { [] }
  | expr COMMA expr_list  { $1::$3 }

args_opt:
    { [] }
  | args { $1 }

args:
    expr  { [$1] }
  | expr COMMA args { $1::$3 }


