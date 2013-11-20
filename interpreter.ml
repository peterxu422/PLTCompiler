open Ast

(*let run f =  f.fname;
	let rec eval = function
		Int(x) -> Int(x)
		| Call("print", [stuff]) ->
		let v = eval stuff in 
			let rec print = function
				Int(x) -> string_of_int x
			in 
				print_endline( print v ); Int(0)
*)

let _ = 
	let lexbuf = Lexing.from_channel stdin in 
	let prog = Parser.program Scanner.token lexbuf in
		print_endline (Ast.string_of_program prog)
		(* print_endline (Ast.string_of_vdecl prog) *)