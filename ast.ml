type op = Add | Sub | Mult | Div | Mod | Lt | Gt | Leq | Geq | Eq | And | Or
type typeConst = Integer | Double | Void | Pitch | Sound

type expr = 
    Int of int
    | Dbl of float
    | Id of string
    | Neg of expr
    | Not of expr
    | Binop of expr * op * expr
    | Assign of string * expr
    | Call of string * expr list
    | Array of string * expr

type stmt = 
    Block of stmt list
    | Expr of expr
    | Return of expr
    | If of expr * stmt * stmt (* How to account for Block([]) *)
    | While of expr * stmt
    (* vdecl? *)

(* type func_decl = {
    rettype : typeConst; (* ? *)
    fname   : string;
    formals : string list;
    locals  : string list;
    body    : stmt list;
} *)

(*?*)
(* type var_decl = {
    vartype : typeConst;
    varname : string;
}
 *)
(* type program = var_decl list * func_decl list *)

(* let string_of_expr = function
    Int(l) -> string_of_int l
    | Dbl(l) -> string_of_float l
    | Id(s) -> s
    | Binop(e1, o, e2) ->
            string_of_expr e1 ^ " " ^
            (match o with
                Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
              | Mod -> "%" | Lt -> "<" | Gt -> ">" | Leq -> "<="
              | Geq -> ">=" | And -> "&" | Or -> "|") ^ " "
              string_of_expr e2
    | Assign(v, e) -> v ^ " = " ^ string_of_expr e
    | Call(f, el) -> 
            f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    | Noexpr -> ""

let string_of_stmt = function
    Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
    | Expr(expr) -> string_of_expr expr ^ ";\n";
    | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
    | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
    | If(e, s1, s2) -> "if (" ^ string_of_expr e ^ ")\n" ^
        string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
    | While(e, s) -> "while (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s

let string_of_typeConst = function
    Integer -> "int"
    | Double -> "double"
    | Void -> "void"
    | Pitch -> "pitch"
    | Sound -> "sound"

let string_of_vdecl vdecl = 
    string_of_typeConst vdecl.vartype ^ vdecl.varname ^ ";\n" 

let string_of_fdecl fdecl =
    string_of_typeConst fdecl.rettype ^ fdecl.fname ^ "(" ^ String.concat
    ", " fdecl.formals ^ ")\n{\n" ^
    String.concat "" (List.map string_of_vdecl fdecl.locals) ^
    String.concat "" (List.map string_of_stmt fdecl.body) ^
    "}\n"

let string_of_program (vars, funcs) =
    String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
    String.concat "\n" (List.map string_of_fdecl funcs) *)