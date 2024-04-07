%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ AND OR NOT NEG
%token IF ELSE WHILE INT BOOL
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
/*TODO: separate definitions and decls into two files*/
program:
  trml decls EOF { let (ta, tb, tc)=$1 in let (da, db)=$2 in (ta, tb, tc, da, db) }

/*TRML - define node types, define nodes, initialize nodes*/
trml:
  TRML LBRACE trmldecls RBRACE { $3 }
trmldecls:
   { ([], [], []) }
 | nodedecl SEMI trmldecls { let (a,b,c) = $3 in (($1 :: a), b, c) }
 | linkdecl SEMI trmldecls { let (a,b,c) = $3 in (a, ($1 :: b), c) }
 | valsdecl SEMI trmldecls { let (a,b,c) = $3 in (a, b, ($1 :: c)) }

/*Node Type Types*/
ntyp:
    ROOT	{ Root }
  | NODE	{ Node }

/*Node Information*/
/*node type declaration, node declaration, node initialization*/
nodedecl:
   ntyp ID 	{ ($1, $2) }
linkdecl:
   ID ID RARROW expr	{ ($1, $2, Eopt($4)) }
 | ID ID	{ ($1, $2, None) }
valsdecl:
   ID ASSIGN expr BTICK expr	{ ($1, $3, Eopt($5)) }
 | ID ASSIGN expr	{ ($1, $3, None) }

/*Declarations*/
decls:
   { ([], []) }
 | vdecl SEMI decls	{ (($1 :: fst $3), snd $3) }
 | fdecl decls	{ (fst $2, ($1 :: snd $2)) }

vdecl_list:
	{ [] }
  | vdecl SEMI vdecl_list  { $1 :: $3 }

vdecl:
  typ ID { ($1, $2) }

typ:
    INT   { Int }
  | BOOL  { Bool }

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
    expr SEMI                               { Expr $1 }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If ($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5) }
  | RETURN expr SEMI                        { Return $2 }

expr:
    LITERAL         	{ Literal($1) }
  | BLIT            	{ BoolLit($1) }
  | ID					{ Id($1) }
  | expr PLUS expr		{ Binop($1, Add, $3) }
  | expr MINUS expr		{ Binop($1, Sub, $3) }
  | expr TIMES expr		{ Binop($1, Tim, $3) }
  | expr DIVIDE expr	{ Binop($1, Div, $3) }
  | expr EQ expr		{ Binop($1, Equal, $3) }
  | expr NEQ expr 		{ Binop($1, Neq, $3) }
  | expr LT expr 		{ Binop($1, Less, $3) }
  | expr LEQ expr 		{ Binop($1, Leq, $3) }
  | expr GT expr 		{ Binop($1, Great, $3) }
  | expr GEQ expr 		{ Binop($1, Geq, $3) }
  | expr AND expr 		{ Binop($1, And, $3) }
  | expr OR expr 		{ Binop($1, Or, $3) }
  | NOT	expr			{ Unop(Not, $2) }
  | ID ASSIGN expr  	{ Assign($1, $3) }
  | LPAREN expr RPAREN	{ $2 }
  | ID LPAREN args_opt RPAREN	{ Call ($1, $3) }

args_opt:
	{ [] }
  | args { $1 }

args:
    expr  { [$1] }
  | expr COMMA args { $1::$3 }
