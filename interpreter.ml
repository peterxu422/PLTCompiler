open Ast 

module NameMap = Map.Make(struct
	type t = string
	let compare x y = Pervasives.compare x y
end)

let getType v = 
	match v with
		Int(v) -> "int"
		| Double(v) -> "double"
		| Boolean(v) -> "bool"

let getInt v = 
	match v with
		Int(v) -> v
		| _ -> 0

let getDouble v =
	match v with
		Double(v) -> v
		| _ -> 0.0

let getBoolean v =
	match v with
		Boolean(v) -> v
		| _ -> false

let initType t = 
  match t with
    "int" -> Int(0)
    | "double" -> Double(0.0)
    | "bool" -> Boolean(false)
    | _ -> Boolean(false)

let run (vars, funcs) =

	(* Put function declarations in a symbol table *)
	let func_decls = List.fold_left
		(fun funcs fdecl -> NameMap.add fdecl.fname fdecl funcs)
		NameMap.empty funcs
	in
	(* set up the function that decomposes a function call *)
	let rec call fdecl actuals globals = 

		(* evals expressions and updates globals *)
		let rec eval env = function

			| Int(i) -> Int i, env
			| Double(i) -> Double i, env
			| Boolean(i) -> Boolean i, env
			(* THIS ISNT RIGHT *)
			| Id(var) -> Id(var), env

(* 			| Assign(var, e) ->
				let v1, env = eval env var in
				let e1, (locals, globals) = eval env e in *)


			| Binop(e1, op, e2) ->
				let v1, env = eval env e1 in
				let v2, env = eval env e2 in
				let v1Type = getType v1 in
				let v2Type = getType v2 in

				if v1Type = v2Type then
					(match op with
						Add -> 
							if v1Type = "int" then
								Int (getInt v1 + getInt v2)
							else if v1Type = "double" then
								Double (getDouble v1 +. getDouble v2)
							else raise (Failure (v1Type ^ " has no + operator"))
						| Sub -> 
							if v1Type = "int" then
								Int (getInt v1 - getInt v2)
							else if v1Type = "double" then
								Double (getDouble v1 -. getDouble v2)
							else raise (Failure (v1Type ^ " has no - operator"))
						| Mult ->
							if v1Type = "int" then
								Int (getInt v1 * getInt v2)
							else if v1Type = "double" then
								Double (getDouble v1 *. getDouble v2)
							else raise (Failure (v1Type ^ " has no * operator"))
						| Div ->
							if v1Type = "int" then
								Int (getInt v1 / getInt v2)
							else if v1Type = "double" then
								Double (getDouble v1 /. getDouble v2)
							else raise (Failure (v1Type ^ " has no / operator"))
						| Mod ->
							if v1Type = "int" then
								Int (getInt v1 mod getInt v2)
							else raise (Failure (v1Type ^ " has no % operator"))
						| Or -> 
							if v1Type = "bool" then
								Boolean (getBoolean v1 || getBoolean v2)
							else raise (Failure (v1Type ^ " has no || operator"))
						| And -> 
							if v1Type = "bool" then
								Boolean (getBoolean v1 && getBoolean v2)
							else raise (Failure (v1Type ^ " has no && operator"))
						| Eq -> 
							if v1Type = "int" then
								Boolean (getInt v1 = getInt v2)
							else if v1Type = "double" then
								Boolean (getDouble v1 = getDouble v2)
							else if v1Type = "bool" then
								Boolean (getBoolean v1 = getBoolean v2)
							else raise (Failure (v1Type ^ " has no == operator"))
						| Neq -> 
							if v1Type = "int" then
								Boolean (getInt v1 <> getInt v2)
							else if v1Type = "double" then
								Boolean (getDouble v1 <> getDouble v2)
							else if v1Type = "bool" then
								Boolean (getBoolean v1 <> getBoolean v2)
							else raise (Failure (v1Type ^ " has no != operator"))
						| Lt ->
							if v1Type = "int" then
								Boolean (getInt v1 < getInt v2)
							else if v1Type = "double" then
								Boolean (getDouble v1 < getDouble v2)
							else raise (Failure (v1Type ^ " has no < operator"))
						| Gt ->
							if v1Type = "int" then
								Boolean (getInt v1 > getInt v2)
							else if v1Type = "double" then
								Boolean (getDouble v1 > getDouble v2)
							else raise (Failure (v1Type ^ " has no > operator"))
						| Leq ->
							if v1Type = "int" then
								Boolean (getInt v1 <= getInt v2)
							else if v1Type = "double" then
								Boolean (getDouble v1 <= getDouble v2)
							else raise (Failure (v1Type ^ " has no <= operator"))
						| Geq ->
							if v1Type = "int" then
								Boolean (getInt v1 >= getInt v2)
							else if v1Type = "double" then
								Boolean (getDouble v1 >= getDouble v2)
							else raise (Failure (v1Type ^ " has no * operator"))
						), env
				else raise (Failure ("Types " ^ v1Type ^ " and " ^ v2Type ^ " do not match"))


			(* our special print function, only supports ints right now *)
			| Call("print", [e]) -> 
				let v, _ = eval env e in
					print_endline ("in print");
					print_endline (Ast.string_of_expr v);
					Boolean(false), env

			(* this does function calls. currently doesn't eval arguments,
			   update variables, etc. It just sets fdecl, then we define a 
			   function called exec that executes statements. Then, we go 
			   through all the statements in the function definition and
			   call exec on them, which pattern matches below. We hit the
			   print() function, which gets evaled in the Expr match, which
			   hits the Call("print", [e]) match  
			*)
			| Call(f, actuals) -> 
				let fdecl =
				  try NameMap.find f func_decls
				  with Not_found -> raise (Failure ("undefined function " ^ f))
				in
				let actuals, env = List.fold_left
					(fun (actuals, env) actual ->
						print_endline (Ast.string_of_expr actual);
					let v, env = eval env actual in v :: actuals, env)
					([], env) actuals
				in
				let (locals, globals) = env in
					let globals = call fdecl (List.rev actuals) []
					in Boolean(false), (locals, globals)
			in

			(* executes statements, calls evals on expressions *)
			let rec exec env = function
				| Expr(e) -> let _, env = eval env e in env
			in  

			(* do locals yo *)
			let locals = 
				try List.fold_left2
					(fun locals formal actual ->
					NameMap.add formal.paramname actual locals)
				NameMap.empty fdecl.formals actuals
				with Invalid_argument(_) ->
					raise (Failure ("wrong number of arguments to: " ^ fdecl.fname))
			in 
			let locals = List.fold_left (* init locals to 0 *)
				(fun locals local -> NameMap.add local.varname (initType local.vartype) locals)
				locals fdecl.locals
			in


			(* this should actually take in env eventually. I think
			   that the fold left will accumulate (locals, globals),
			   which will be returned by stuff above*)
	    	snd (List.fold_left exec (locals, globals) fdecl.body)

		in
		(* needs globals i think *)
	    call (NameMap.find "main" func_decls) [] []

let _ = 
	let lexbuf = Lexing.from_channel stdin in 
	(*let stmt = Parser.stmt Scanner.token lexbuf in*)
	let program = Parser.program Scanner.token lexbuf in
	run program
	(*	print_endline (Ast.string_of_program program) *)
	
