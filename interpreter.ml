open Ast 

module NameMap = Map.Make(struct
	type t = string
	let compare x y = Pervasives.compare x y
end)

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
			| Int(i) -> i, env
			(* our special print function, only supports ints right now *)
			| Call("print", [e]) -> 
				let v, _ = eval env e in
					print_endline ("in print");
					print_endline (string_of_int v);
					0, env

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
				let globals = call fdecl [] []
				in
		  	 0, env
		in

		(* executes statements, calls evals on expressions *)
		let rec exec env = function
			| Expr(e) -> let _ = eval env e in env
		in  

		(* this should actually take in env eventually. I think
		   that the fold left will accumulate (locals, globals),
		   which will be returned by stuff above*)
		print_endline ("calling " ^ fdecl.fname );
		print_endline ("body:  " ^ string_of_fdecl fdecl);
    	List.fold_left exec () fdecl.body

	in
    call (NameMap.find "main" func_decls) [] []

let _ = 
	let lexbuf = Lexing.from_channel stdin in 
	let program = Parser.program Scanner.token lexbuf in
	run program
