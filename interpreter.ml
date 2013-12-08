open Ast 
open Printf
open Helper

module NameMap = Map.Make(struct
	type t = string
	let compare x y = Pervasives.compare x y
end)

let getType v = 
	match v with
		Int(v) -> "int"
		| Double(v) -> "double"
		| Boolean(v) -> "bool"
		| Pitch(v) -> "pitch"
		| Sound(p,d,a) -> "sound"
		| _ -> "unmatched_type"

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

let getPitch v =
	match v with
		Pitch(v) -> v
		| _ -> "C0"

let getSound v =
	match v with
		Sound(p,d,a) -> (p,d,a)
		| _ -> (["C0"], 0., 0)

exception ReturnException of expr * expr NameMap.t

let initType t = 
  match t with
    "int" -> Int(0)
    | "double" -> Double(0.0)
    | "bool" -> Boolean(false)
    | "pitch" -> Pitch("C0")
    | "sound" -> Sound((["C0"], 0., 0))
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
			 Int(i) -> Int(i), env
			| Double(d) -> Double(d), env
			| Boolean(b) -> Boolean(b), env
			| Pitch(p) -> Pitch(p), env
			| Sound(p,d,a) -> Sound(p,d,a), env
			| Index(a,i) -> let v, (locals, globals) = eval env (Id(a)) in
				let rec lookup arr indices =
					let arr = match arr with
						Array(n) -> n
						| _ -> raise (Failure(a ^ " is not an array. Cannot access index"))
					in
					match indices with
					[] -> raise (Failure ("Error indexing array without indices"))
					| Int(i) :: [] -> 
					try
						List.nth arr i, env
					with Failure("nth") -> raise (Failure "Index out of bounds")
					| _ -> raise (Failure "Invalid index")
				in
				lookup v i
			| Id(var) -> 
			let locals, globals = env in
			if NameMap.mem var locals then
				(NameMap.find var locals), env
			else if NameMap.mem var globals then
				(NameMap.find var globals), env
			else raise (Failure ("undeclared identifier " ^ var))
 			| Assign(var, e) ->
			let v, (locals, globals) = eval env e in
			(match var with
				Id(name) ->
				(* The local identifiers have already been added to ST in the first pass. 
				Checks if it is indeed in there.*)
				if NameMap.mem name locals then	
				(* Updates the var in the ST to evaluated expression e, which is stored in v. 
				Returns v as the value because this is the l-value*)
					v, (NameMap.add name v locals, globals) 
				else if NameMap.mem name globals then
					v, (locals, NameMap.add name v globals)
				else raise (Failure ("undeclared identifier " ^ name))
				| Index(name, indices) -> 
					let rec getIndex e = 
						let v, env = eval env e in 
						(match v with
							Int(i) -> i
							(*| Id(i) -> let idx, v = eval env (Id(i)) in getIndex idx*) (*Need to call getIndexFromVar again because function needs to return only 1 value*)
							| e ->
								print_endline (string_of_expr e);
							 	raise (Failure ("Illegal index"))
					)
					in
					let rec setElt exprs = function
						[] -> raise (Failure ("Cannot assign to empty array"))
						| hd :: [] -> let idx = getIndex hd in
							if idx < (List.length exprs) then
								let arr = (Array.of_list exprs) in arr.(idx) <- v; Array.to_list arr
							else

								(* TODO: have this init with the initType of the array *)
								let arr = 
								(Array.append 
									(Array.of_list exprs) 
									(Array.make (1+idx-(List.length exprs)) 
										(initType (getType (v))))) 
								in 
									arr.(idx) <- v; Array.to_list arr
						in
					if NameMap.mem name locals then
						let exprList = (match (NameMap.find name locals) with
							Array(a) -> a
							| _ -> raise (Failure (name ^ " is not an array"))) in
						let newArray = Array(setElt exprList indices) in
						v, (NameMap.add name newArray locals, globals)
					else if (NameMap.mem name globals) then
						let exprList = (match (NameMap.find name locals) with
							Array(a) -> a
							| _ -> raise (Failure (name ^ " is not an array"))) in
						let newArray = Array(setElt exprList indices) in
						v, (locals, NameMap.add name newArray globals)
					else
						raise (Failure (name ^ " was not properly initialized as an array"))
			| _ -> raise (Failure ("Can only assign variables or array indices")))

			(* binop operators *)
			| Binop(e1, op, e2) ->
				let v1, env = eval env e1 in
				let v2, env = eval env e2 in
				let v1Type = getType v1 in
				let v2Type = getType v2 in
				(match op with
					(* v1 + v2 *)
					Add -> 
						if v1Type = "int" then
							(if v2Type = "double" then
								Double ((float_of_int (getInt v1)) +. getDouble v2)
							else if v2Type = "pitch" then
								Pitch (intToPitch(getInt v1 + pitchToInt (getPitch v2)))
							else if v2Type = "int" then
								Int (getInt v1 + getInt v2)
							else raise (Failure (v1Type ^ " + " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "double" then
							(if v2Type = "int" then
								Double (getDouble v1 +. (float_of_int (getInt v2)))
							else if v2Type = "double" then
								Double (getDouble v1 +. getDouble v2)
							else raise (Failure (v1Type ^ " + " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "pitch" then
							(if v2Type = "int" then
								Pitch (intToPitch(pitchToInt (getPitch v1) + getInt v2))
							else raise (Failure (v1Type ^ " + " ^ v2Type ^ " is not a valid operation")))
						else raise (Failure (v1Type ^ " + " ^ v2Type ^ " is not a valid operation"))
					(* v1 - v2 *)
					| Sub -> 
						if v1Type = "int" then
							(if v2Type = "double" then
								Double ((float_of_int (getInt v1)) -. getDouble v2)
							else if v2Type = "pitch" then
								Pitch (intToPitch(getInt v1 - pitchToInt (getPitch v2)))
							else if v2Type = "int" then
								Int (getInt v1 - getInt v2)
							else raise (Failure (v1Type ^ " - " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "double" then
							(if v2Type = "int" then
								Double (getDouble v1 -. (float_of_int (getInt v2)))
							else if v2Type = "double" then
								Double (getDouble v1 -. getDouble v2)
							else raise (Failure (v1Type ^ " - " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "pitch" then
							(if v2Type = "int" then
								Pitch (intToPitch(pitchToInt (getPitch v1) - getInt v2))
							else raise (Failure (v1Type ^ " - " ^ v2Type ^ " is not a valid operation")))
						else raise (Failure (v1Type ^ " - " ^ v2Type ^ " is not a valid operation"))
					(* v1 * v2 *)
					| Mult ->
						if v1Type = "int" then
(* 			| Call("getDuration", [e]) ->
				let v, env = eval env e in
				(match v with
					  Sound(p,d,a) -> Double(d), env
					| _ -> raise (Failure ("getDuration can only be called on sounds"))
				) *)
							(if v2Type = "sound" then
								(match v2 with
									Sound(p,d,a) -> Sound(p,float_of_int (getInt v1) *. d, a)
									| _ -> raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
							else if v2Type = "double" then
								Double ((float_of_int (getInt v1)) *. getDouble v2)
							else if v2Type = "pitch" then
								Pitch (intToPitch(getInt v1 * pitchToInt (getPitch v2)))
							else if v2Type = "int" then
								Int (getInt v1 * getInt v2)
							else raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "double" then
							(if v2Type = "sound" then
								(match v2 with
									Sound(p,d,a) -> Sound(p,getDouble v1 *. d, a)
									| _ -> raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
							else if v2Type = "int" then
								Double (getDouble v1 *. (float_of_int (getInt v2)))
							else if v2Type = "double" then
								Double (getDouble v1 *. getDouble v2)
							else raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "pitch" then
							(if v2Type = "int" then
								Pitch (intToPitch(pitchToInt (getPitch v1) * getInt v2))
							else raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "sound" then
							(if v2Type = "int" then
								(match v1 with
									Sound(p,d,a) -> Sound(p,d *. float_of_int (getInt v2),a)
									| _ -> raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
							else if v2Type = "double" then
								(match v1 with
									Sound(p,d,a) -> Sound(p,d *. getDouble v2,a)
									| _ -> raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
							else raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
						else raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation"))
					(* v1 / v2 *)
					| Div ->
						if v1Type = "int" then
							(if v2Type = "double" then
								Double ((float_of_int (getInt v1)) /. getDouble v2)
							else if v2Type = "pitch" then
								Pitch (intToPitch(getInt v1 / pitchToInt (getPitch v2)))
							else if v2Type = "int" then
								Int (getInt v1 / getInt v2)
							else raise (Failure (v1Type ^ " / " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "double" then
							(if v2Type = "int" then
								Double (getDouble v1 /. (float_of_int (getInt v2)))
							else if v2Type = "double" then
								Double (getDouble v1 /. getDouble v2)
							else raise (Failure (v1Type ^ " / " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "pitch" then
							(if v2Type = "int" then
								Pitch (intToPitch(pitchToInt (getPitch v1) / getInt v2))
							else raise (Failure (v1Type ^ " / " ^ v2Type ^ " is not a valid operation")))
						else raise (Failure (v1Type ^ " / " ^ v2Type ^ " is not a valid operation"))
					(* v1 % v2 *)
					| Mod ->
						if v1Type = "int" then
							(if v2Type = "int" then
								Int (getInt v1 mod getInt v2)
							else if v2Type = "pitch" then
								Pitch (intToPitch(getInt v1 mod pitchToInt (getPitch v2)))
							else raise (Failure (v1Type ^ " % " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "pitch" then
							(if v2Type = "int" then
								Pitch (intToPitch(pitchToInt (getPitch v1) mod getInt v2))
							else raise (Failure (v1Type ^ " % " ^ v2Type ^ " is not a valid operation")))
						else raise (Failure (v1Type ^ " % " ^ v2Type ^ " is not a valid operation"))
					(* v1 || v2 *)
					| Or -> 
						if v1Type = "bool" && v2Type = "bool" then
							Boolean (getBoolean v1 || getBoolean v2)
						else raise (Failure (v1Type ^ " || " ^ v2Type ^ " is not a valid operation"))
					(* v1 && v2 *)
					| And -> 
						if v1Type = "bool" && v2Type = "bool" then
							Boolean (getBoolean v1 && getBoolean v2)
						else raise (Failure (v1Type ^ " && " ^ v2Type ^ " is not a valid operation"))
					(* v1 == v2 *)
					| Eq -> 
						if v1Type = "int" then
							(if v2Type = "double" then
								Boolean ((float_of_int (getInt v1)) = getDouble v2)
							else if v2Type = "pitch" then
								Boolean (getInt v1 = pitchToInt (getPitch v2))
							else if v2Type = "int" then
								Boolean (getInt v1 = getInt v2)
							else raise (Failure (v1Type ^ " == " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "double" then
							(if v2Type = "int" then
								Boolean (getDouble v1 = (float_of_int (getInt v2)))
							else if v2Type = "double" then
								Boolean (getDouble v1 = getDouble v2)
							else raise (Failure (v1Type ^ " == " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "pitch" then
							(if v2Type = "pitch" then
								Boolean (pitchToInt (getPitch v1) = pitchToInt (getPitch v2))
							else if v2Type = "int" then
								Boolean (pitchToInt (getPitch v1) = getInt v2)
							else raise (Failure (v1Type ^ " == " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "bool" && v2Type = "bool" then
							Boolean (getBoolean v1 = getBoolean v2)
						else raise (Failure (v1Type ^ " == " ^ v2Type ^ " is not a valid operation"))
					(* v1 != v2 *)
					| Neq -> 
						if v1Type = "int" then
							(if v2Type = "double" then
								Boolean ((float_of_int (getInt v1)) <> getDouble v2)
							else if v2Type = "pitch" then
								Boolean (getInt v1 <> pitchToInt (getPitch v2))
							else if v2Type = "int" then
								Boolean (getInt v1 <> getInt v2)
							else raise (Failure (v1Type ^ " != " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "double" then
							(if v2Type = "int" then
								Boolean (getDouble v1 <> (float_of_int (getInt v2)))
							else if v2Type = "double" then
								Boolean (getDouble v1 <> getDouble v2)
							else raise (Failure (v1Type ^ " != " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "pitch" then
							(if v2Type = "pitch" then
								Boolean (pitchToInt (getPitch v1) <> pitchToInt (getPitch v2))
							else if v2Type = "int" then
								Boolean (pitchToInt (getPitch v1) <> getInt v2)
							else raise (Failure (v1Type ^ " != " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "bool" && v2Type = "bool" then
							Boolean (getBoolean v1 <> getBoolean v2)
						else raise (Failure (v1Type ^ " != " ^ v2Type ^ " is not a valid operation"))
					(* v1 < v2 *)
					| Lt ->
						if v1Type = "int" then
							(if v2Type = "double" then
								Boolean ((float_of_int (getInt v1)) < getDouble v2)
							else if v2Type = "pitch" then
								Boolean (getInt v1 < pitchToInt (getPitch v2))
							else if v2Type = "int" then
								Boolean (getInt v1 < getInt v2)
							else raise (Failure (v1Type ^ " < " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "double" then
							(if v2Type = "int" then
								Boolean (getDouble v1 < (float_of_int (getInt v2)))
							else if v2Type = "double" then
								Boolean (getDouble v1 < getDouble v2)
							else raise (Failure (v1Type ^ " < " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "pitch" then
							(if v2Type = "pitch" then
								Boolean (pitchToInt (getPitch v1) < pitchToInt (getPitch v2))
							else if v2Type = "int" then
								Boolean (pitchToInt (getPitch v1) < getInt v2)
							else raise (Failure (v1Type ^ " < " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "bool" && v2Type = "bool" then
							Boolean (getBoolean v1 < getBoolean v2)
						else raise (Failure (v1Type ^ " < " ^ v2Type ^ " is not a valid operation"))
					(* v1 > v2 *)
					| Gt ->
						if v1Type = "int" then
							(if v2Type = "double" then
								Boolean ((float_of_int (getInt v1)) > getDouble v2)
							else if v2Type = "pitch" then
								Boolean (getInt v1 > pitchToInt (getPitch v2))
							else if v2Type = "int" then
								Boolean (getInt v1 > getInt v2)
							else raise (Failure (v1Type ^ " > " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "double" then
							(if v2Type = "int" then
								Boolean (getDouble v1 > (float_of_int (getInt v2)))
							else if v2Type = "double" then
								Boolean (getDouble v1 > getDouble v2)
							else raise (Failure (v1Type ^ " > " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "pitch" then
							(if v2Type = "pitch" then
								Boolean (pitchToInt (getPitch v1) > pitchToInt (getPitch v2))
							else if v2Type = "int" then
								Boolean (pitchToInt (getPitch v1) > getInt v2)
							else raise (Failure (v1Type ^ " > " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "bool" && v2Type = "bool" then
							Boolean (getBoolean v1 > getBoolean v2)
						else raise (Failure (v1Type ^ " > " ^ v2Type ^ " is not a valid operation"))
					(* v1 <= v2 *)
					| Leq ->
						if v1Type = "int" then
							(if v2Type = "double" then
								Boolean ((float_of_int (getInt v1)) <= getDouble v2)
							else if v2Type = "pitch" then
								Boolean (getInt v1 <= pitchToInt (getPitch v2))
							else if v2Type = "int" then
								Boolean (getInt v1 <= getInt v2)
							else raise (Failure (v1Type ^ " <= " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "double" then
							(if v2Type = "int" then
								Boolean (getDouble v1 <= (float_of_int (getInt v2)))
							else if v2Type = "double" then
								Boolean (getDouble v1 <= getDouble v2)
							else raise (Failure (v1Type ^ " <= " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "pitch" then
							(if v2Type = "pitch" then
								Boolean (pitchToInt (getPitch v1) <= pitchToInt (getPitch v2))
							else if v2Type = "int" then
								Boolean (pitchToInt (getPitch v1) <= getInt v2)
							else raise (Failure (v1Type ^ " <= " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "bool" && v2Type = "bool" then
							Boolean (getBoolean v1 <= getBoolean v2)
						else raise (Failure (v1Type ^ " <= " ^ v2Type ^ " is not a valid operation"))
					(* v1 >= v2 *)
					| Geq ->
						if v1Type = "int" then
							(if v2Type = "double" then
								Boolean ((float_of_int (getInt v1)) >= getDouble v2)
							else if v2Type = "pitch" then
								Boolean (getInt v1 >= pitchToInt (getPitch v2))
							else if v2Type = "int" then
								Boolean (getInt v1 >= getInt v2)
							else raise (Failure (v1Type ^ " >= " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "double" then
							(if v2Type = "int" then
								Boolean (getDouble v1 >= (float_of_int (getInt v2)))
							else if v2Type = "double" then
								Boolean (getDouble v1 >= getDouble v2)
							else raise (Failure (v1Type ^ " >= " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "pitch" then
							(if v2Type = "pitch" then
								Boolean (pitchToInt (getPitch v1) >= pitchToInt (getPitch v2))
							else if v2Type = "int" then
								Boolean (pitchToInt (getPitch v1) >= getInt v2)
							else raise (Failure (v1Type ^ " >= " ^ v2Type ^ " is not a valid operation")))
						else if v1Type = "bool" && v2Type = "bool" then
							Boolean (getBoolean v1 >= getBoolean v2)
						else raise (Failure (v1Type ^ " >= " ^ v2Type ^ " is not a valid operation"))
					), env

				(* else raise (Failure ("Types " ^ v1Type ^ " and " ^ v2Type ^ " do not match")) *)

				(* !e *)
				| Not(e) ->
					let v, env = eval env e in
					let vType = getType v in
					if vType = "bool" then
						Boolean (not (getBoolean v)), env
					else raise (Failure (vType ^ " has no ! operator"))

				(* -e *)
				| Neg(e) ->
					let v, env = eval env e in
					let vType = getType v in
					if vType = "int" then
						Int (0 - getInt v), env
					else if vType = "double" then
						Double (0. -. getDouble v), env
					else raise (Failure (vType ^ " has no - operator"))

			(* Arrays *)
			| Array(e) -> Array(e), env
			(* our special print function, only supports ints right now *)
			| Call("print", [e]) -> 
				let v, env = eval env e in
				
				let rec print = function
					Int(i) -> string_of_int i
					| Double(d) -> string_of_float d
					| Boolean(b) -> string_of_bool b
					| Pitch(p) -> p
					| Sound(p,d,a) -> "|" ^ String.concat ", " p ^ "|:" ^ string_of_float d ^ ":" ^ string_of_int a
					| Array(a) -> "[" ^ build a ^ "]" and build = function
							hd :: [] -> (print hd)
							| hd :: tl -> ((print hd) ^ ", " ^ (build tl))
					| _ -> raise (Failure ("Item cannot be printed"))
				in
					print_endline (print v);
					Int(0), env
					(*
					print_endline ("in print");
					print_endline (Ast.string_of_expr v);
					Boolean(false), env *)

			| Call ("mixdown", [e]) ->
				let v, env = eval env e in
				let file = "bytecode" in
				let oc = open_out file in
				let rec writeByteCode = function
					Sound(p,d,a) -> "[" ^ String.concat ", " p ^ "]:" ^ string_of_float d ^ ":" ^ string_of_int a
					| Array(a) -> "[" ^ build a ^ "]" and build = function
							hd :: [] -> (writeByteCode hd)
							| hd :: tl -> ((writeByteCode hd) ^ "," ^ (build tl))
					| _ -> raise (Failure ("Item cannot be mixdown"))
				in 
					(* let append_string path s =
    					let chan = open_out_gen [Open_wronly; Open_creat] 0o666 path
    					in let len = out_channel_length chan
    					in
    					    begin
    					    seek_out chan len;
    					    output_string chan s;
    					    close_out chan;
    					    end
    				append_string file (writeByteCode v); *)
					(*  *)
					fprintf oc "%s\n" (writeByteCode v);
					flush oc;
					(*  *)
					
					Int(0), env
			(* for pitches and sounds *)
			| Call("getAmplitude", [e]) ->
				let v, env = eval env e in
				(match v with
					  Sound(p,d,a) -> Int(a), env
					| _ -> raise (Failure ("getAmplitude can only be called on sounds"))
				)
			(* for sounds *)
			| Call("getDuration", [e]) ->
				let v, env = eval env e in
				(match v with
					  Sound(p,d,a) -> Double(d), env
					| _ -> raise (Failure ("getDuration can only be called on sounds"))
				)
			(* for pitches and sounds *)
			| Call("getPitch", [e]) ->
				let v, env = eval env e in
				(match v with
					  Sound(p,d,a) -> 
					  let rec strings_to_pitches = function
					  		  hd :: [] -> [Pitch(hd)]
					  		| hd :: tl -> [Pitch(hd)] @ strings_to_pitches tl
					  in
					  Array(List.rev(strings_to_pitches p)), env
					| Pitch(p) -> Pitch(p), env
					| _ -> raise (Failure ("getPitch can only be called on sounds or pitches"))
				)
			(* for arrays eyes only *)
			| Call("length",[e]) -> 
				let v, env = eval env e in
				(match v with
					  Array(a) -> Int(List.length a), env
					| _ -> raise (Failure ("Length can only be called on arrays"))
				)
			(* this does function calls. *)
			| Call(f, actuals) -> 
				let fdecl =
				  try NameMap.find f func_decls
				  with Not_found -> raise (Failure ("undefined function " ^ f))
				in
				let actuals, env = List.fold_left
					(fun (actuals, env) actual ->
						print_endline (Ast.string_of_expr actual);
					let v, env = eval env actual in v :: actuals, env)
					([], env) (List.rev actuals)
				in
				let (locals, globals) = env in
				try
					let globals = call fdecl actuals globals
					in Boolean(false), (locals, globals)
				with ReturnException(v, globals) -> v, (locals, globals)
			in

			(* executes statements, calls evals on expressions *)
			let rec exec env = function
			Block(stmts) -> List.fold_left exec env stmts
				| Expr(e) -> let _, env = eval env e in env
				| If(e, s1, s2) ->
				let v, env = eval env e in
				exec env (if getBoolean v !=  false then s1 else s2)
				| While(e, s) ->
				let rec loop env =
					let v, env = eval env e in
					if getBoolean v != false then loop (exec env s) else env
				in loop env
				| For(e1, e2, e3, s) ->
				let _, env = eval env e1 in
				let rec loop env =
					let v, env = eval env e2 in
					if getBoolean v != false then
					  let _, env = eval (exec env s) e3 in
					  loop env
					else
						env
				in loop env
				| Return(e) ->
				let v, (locals, globals) = eval env e in
				raise (ReturnException(v, globals))
			in  

			(* Enter the function: bind actual values to formal arguments *)
			let locals = 
				try List.fold_left2
					(fun locals formal actual -> NameMap.add formal.paramname actual locals) NameMap.empty fdecl.formals actuals
				with Invalid_argument(_) ->
					raise (Failure ("wrong number of arguments to: " ^ fdecl.fname))
			in 
			let locals = List.fold_left (* init locals to 0 *)
				(fun locals local -> NameMap.add local.varname (initType local.vartype) locals) locals fdecl.locals
			in
			(* this should actually take in env eventually. I think
			   that the fold left will accumulate (locals, globals),
			   which will be returned by stuff above*)
	    	snd (List.fold_left exec (locals, globals) fdecl.body)

		in let globals = List.fold_left
			(fun globals vdecl -> NameMap.add vdecl.varname (initType vdecl.vartype) globals) NameMap.empty vars
		in try
		(* needs globals i think *)
			call (NameMap.find "main" func_decls) [] globals
		with Not_found -> raise (Failure("did not find the main() function"))

let _ = 
	let lexbuf = Lexing.from_channel stdin in 
	let program = Parser.program Scanner.token lexbuf in
	run program

		(*print_endline (Ast.string_of_program program)*)
