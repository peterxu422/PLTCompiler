open Ast

let rec eval = function
	Int(x) -> Int(x)
	| Call("print", [stuff]) ->
	let v = eval stuff in 
		let rec print = function
			Int(x) -> string_of_int x
		in 
			print_endline( print v ); Int(0)
	

let _ = 
	let lexbuf = Lexing.from_channel stdin in 
	let expr = Parser.expr Scanner.token lexbuf in
	let result = eval expr in 
	result