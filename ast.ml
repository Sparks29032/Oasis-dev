(*Abstract Syntax Tree*)

(*Operations*)
(*TODO: add negation*)
type unop = Not
type binop = Add | Sub | Tim | Div | Equal | Neq | Less | Leq | Great | Geq | And | Or

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
type expr_opt = None | Eopt of expr

(*Statements*)
(*TODO: add for loop*)
(*TODO: add list comprehension*)
type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Return of expr

(*Bindings*)
type bind = typ * string
type nbind = ntyp * string
type lbind = string * string * expr_opt
type vbind = string * expr * expr_opt

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

(*Program*)
type program = nbind list * lbind list * vbind list * bind list * func_def list

(*Pretty-Printing*)
(*Operators*)
let string_of_binop = function
    Add -> "+"
  | Sub -> "-"
  | Tim -> "*"
  | Div -> "/"
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
let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

(*Types*)
let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
let string_of_ntyp = function
    Root -> "ROOT"
  | Node -> "NODE"
let string_of_linkopt = function
	None -> ""
  | Eopt(e) -> " -> " ^ string_of_expr e
let string_of_valsopt = function
	None -> ""
  | Eopt(e) -> ", bval: " ^ string_of_expr e

(*Declarations*)
let string_of_nodedecl (nt, id) = string_of_ntyp nt ^ " " ^ id ^ ";\n"
let string_of_linkdecl (idtyp, iddec, lkopt) = idtyp ^ " " ^ iddec ^ string_of_linkopt lkopt ^ ";\n"
let string_of_valsdecl (id, fval, bval) = id ^ " = < fval: " ^ string_of_expr fval ^ string_of_valsopt bval ^ " >;\n"

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