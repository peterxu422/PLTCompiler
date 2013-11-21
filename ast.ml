type op = Add | Sub | Mult | Div | Mod | Lt | Gt | Leq | Geq | Eq | And | Or
type typeConst = Integer | Double | Void | Pitch | Sound | Boolean

type expr =
	  Int of int
	| Double of float
	| Boolean of bool
	| Pitch of string
	| Id of string
	| Array of expr list
	| Call of string * expr list
	| Assign of expr * expr

type stmt =
	  Block of stmt list
	| Expr of expr
	
type var_decl = {
	varname : string;
	vartype : string;
}

type par_decl = {
	paramname : string; (* Name of the variable *)
	paramtype : string; (* Name of variable type *)
}

type func_decl = {
	rtype : string;
	fname : string;
	formals : par_decl list;
	locals : var_decl list;
	body : stmt list;
}


type program = var_decl list * func_decl list

let rec string_of_expr = function
	  Int(i) -> string_of_int i
	| Double(d) -> string_of_float d
	| Boolean(b) -> string_of_bool b
	| Pitch(p) -> p
	| Id(s) -> s
	| Array(s) ->
		"[" ^ String.concat ", " (List.map string_of_expr s) ^ "]"
	| Call(f, el) -> 
		f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
	| Assign(id, stuff) -> 
		(string_of_expr id) ^ " = " ^ (string_of_expr stuff)


let rec string_of_stmt = function
	Block(stmts) ->
		"{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
	| Expr(expr) ->  string_of_expr expr ^ ";\n"

let string_of_vdecl vdecl = vdecl.vartype ^ " " ^ vdecl.varname ^ ";\n"	
let string_of_pdecl pdecl = pdecl.paramtype ^ " " ^ pdecl.paramname	

let string_of_fdecl fdecl =
	fdecl.rtype ^ " " ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_pdecl fdecl.formals) ^ ")\n{\n" ^
	String.concat "" (List.map string_of_vdecl fdecl.locals) ^
	String.concat "" (List.map string_of_stmt fdecl.body) ^
	"}\n"
	
let string_of_program (vars, funcs) =
	String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
	String.concat "\n" (List.map string_of_fdecl funcs)