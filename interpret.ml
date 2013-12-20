open Ast

module NameMap = Map.Make(struct
	type t = string
	let compare x y = Pervasives.compare x y
end)

type typeConst = Integer of int | Bool of bool

exception ReturnException of typeConst * typeConst NameMap.t

let getType v = 
	match v with
		Int(v) -> "int"
		|Bool(v) -> "bool"

let getInt v =
	match v with
		Int(v) -> v
		| _ -> 0

let getBool v = 
	match v with
		Bool(v) -> v
		| _ -> false


let run (vars, funcs) =
	let func_decls = List.fold_left
		(fun funcs fdecl -> nameMap.add fdecl.fname fdecl funcs)
		NameMap.empty funcs
	in

	let rec call fdecl =

	let rec eval env = function
		  Int(x) -> x, env
		| Call("print", [stuff]) ->
			let v, env = eval env stuff in 
			print_endline(string_of_int v);
			(Bool false), env
		in
		let rec exec env = function
			Block(stmts) -> List.fold_left exec env stmts
		in 
		let globals = List.fold_left
			(fun globals vdecl -> NameMap.add vdecl.varname (initIdentifier vdecl.vartype) globals)
			NameMap.empty vars
		in try
			call (NameMap.find "main" func_decls) [] globals
		with Not_found ->
			raise (Failure ("did not find the main() function"))
