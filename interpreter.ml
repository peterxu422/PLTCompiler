open Ast 

module NameMap = Map.Make(struct
	type t = string
	let compare x y = Pervasives.compare x y
end)

let initType t = 
  match t with
    "int" -> Int(0)
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
			| Int(i) -> Int(i), env
(* 			| Id(var) -> 
				let locals, globals = env in
				if NameMap.mem var locals then
					(NameMap.find var locals), env
				else if NameMap.mem var globals then
					(NameMap.find var globals), env *)
(* 			| Assign(var, e) ->
				let v1, env = eval env var in
				let e1, (locals, globals) = eval env e in *)


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
				print_endline "actuals: ";
				print_endline (Ast.string_of_expr (List.hd actuals));
				let fdecl =
				  try NameMap.find f func_decls
				  with Not_found -> raise (Failure ("undefined function " ^ f))
				in
				let actuals, env = List.fold_left
					(fun (actuals, env) actual ->
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
(* 			print_endline ("calling " ^ fdecl.fname );
			print_endline ("body:  " ^ string_of_fdecl fdecl); *)
	    	(* List.fold_left exec (2) fdecl.body *)
	    	snd (List.fold_left exec (locals, globals) fdecl.body)

		in
	    call (NameMap.find "main" func_decls) [] []

let _ = 
	let lexbuf = Lexing.from_channel stdin in 
	let program = Parser.program Scanner.token lexbuf in
	run program
