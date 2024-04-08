(*Abstract Syntax Tree*)

(*Operations*)
(*TODO: add negation*)
type unop = Not
type binop = Add | Sub | Tim | Div | Mod | Equal | Neq | Less | Leq | Great | Geq | And | Or

(*Types*)
(*TODO: add strings and floats*)
type typ = Int | Bool
type ntyp = Root | Node

(*Expressions*)
type expr =
    Literal of int
  | BoolLit of bool
  | Id of string
  | Unop of unop * expr
  | Binop of expr * binop * expr
  | Assign of string * expr
  | Call of string * expr list

(*Statements*)
(*TODO: list comprehension*)
type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt_opt
  | While of expr * stmt
  | For of string * (expr * expr) list * stmt
  | Return of expr
and stmt_opt = None | StmtOpt of stmt

(*Bindings*)
type bind = typ * string
type val_opt = None | ValOpt of typ * expr
type str_opt = None | StringOpt of string
type nbind = ntyp * string
type lbind = string * string * str_opt
type vbind = string * val_opt * val_opt

(*TRML File Information*)
type trml_obj = {
  nodetyp: nbind list;
  linktyp: lbind list;
  valstyp: vbind list;
}

(*Function Information*)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

(*ForwardFunction Information*)
type fwdfunc_def = {
  pnode : string;
  cnode : string;
  formals : bind list; 
  locals : bind list;
  body : stmt list;
}

(*Program*)
type program = nbind list * lbind list * vbind list * bind list * func_def list

(*Pretty-Printing*)
(*Operators*)
let string_of_binop = function
    Add -> "+"
  | Sub -> "-"
  | Tim -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Great -> ">"
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"
let string_of_unop = function
    Not -> "not"

(*Expressions*)
let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2
  | Unop(o, e) ->
	string_of_unop o ^ " " ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

(*Statements*)
let string_of_forexpr = function
    (e1, e2) -> if e1 = e2 then string_of_expr e1 else string_of_expr e1 ^ " : " ^ string_of_expr e2
let rec string_of_stmt = function
	Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmtopt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(v, el, s) -> "for (" ^ v ^ " in [" ^ String.concat ", " (List.map string_of_forexpr el) ^ "]) " ^ string_of_stmt s
and string_of_stmtopt = function
    StmtOpt(s) -> string_of_stmt s
  | None -> ";\n"

(*TRML Declarations*)
let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
let string_of_ntyp = function
    Root -> "ROOT"
  | Node -> "NODE"
let string_of_linkopt = function
    StringOpt(e) -> " -> " ^ e
  | None -> ""
let string_of_valsopt = function
    ValOpt(t, e) -> string_of_typ t ^ " " ^ string_of_expr e
  | None -> "None"

(*Declarations*)
let string_of_nodedecl (nt, id) = string_of_ntyp nt ^ " " ^ id ^ ";\n"
let string_of_linkdecl (idtyp, iddec, lkopt) = idtyp ^ " " ^ iddec ^ string_of_linkopt lkopt ^ ";\n"
let string_of_valsdecl (id, fvalopt, bvalopt) = id ^ " = < fval: " ^ string_of_valsopt fvalopt ^ ", bval: " ^ string_of_valsopt bvalopt ^ " >;\n"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"
let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

(*Program*)
let string_of_program (nodes, ndcls, ninis, vars, funcs) =
  "\n~~~~~~~~~~~~~\n|Parsed TRML|\n~~~~~~~~~~~~~\n" ^
  String.concat "\n" (List.map string_of_nodedecl nodes) ^ "\n" ^
  String.concat "\n" (List.map string_of_linkdecl ndcls) ^ "\n" ^
  String.concat "\n" (List.map string_of_valsdecl ninis) ^ "\n" ^
  "\n~~~~~~~~~~~~\n|Parsed TRS|\n~~~~~~~~~~~~\n" ^
  String.concat "\n" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
