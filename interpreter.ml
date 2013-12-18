open Ast  open Printf open Helper

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

let getBoolean v =
	match v with
		Boolean(v) -> v
		| _ -> false

exception ReturnException of expr * expr NameMap.t

let initType t = 
  match t with
    "int" -> Int(0)
    | "double" -> Double(0.0)
    | "bool" -> Boolean(false)
    | "pitch" -> Pitch("C0")
    | "sound" -> Sound((["C0"], 0., 0))
    | _ -> Boolean(false)

(* global mixdown flag to see if mixdown has been called in which case we should append, not re write a file *)
let mixdown_flag = ref false;;

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
				(match eval env (Id(name)) with
					Index(a, i), _ -> eval env (Assign((Index(a,i), e)))
					| _,_ ->
					(* The local identifiers have already been added to ST in the first pass. 
					Checks if it is indeed in there.*)
					if NameMap.mem name locals then	
					(* Updates the var in the ST to evaluated expression e, which is stored in v. 
					Returns v as the value because this is the l-value*)
						v, (NameMap.add name v locals, globals) 
					else if NameMap.mem name globals then
						v, (locals, NameMap.add name v globals)
					else raise (Failure ("undeclared identifier " ^ name))
				)
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
				let v1, env = 
				(match eval env e1 with
					Index(a,i), _ ->  eval env (Index(a,i))
					| _,_ -> eval env e1) in
				let v2, env =
				(match eval env e2 with
					Index(a,i), _ -> eval env (Index(a,i))
					| _,_ -> eval env e2) in
				let v1Type = getType v1 in
				let v2Type = getType v2 in
				(match op with
					(* v1 + v2 *)
					Add -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Double (float_of_int i1 +. d2)
							| Pitch(p2) -> Pitch (intToPitch(i1 + pitchToInt p2))
							| Int(i2) -> Int (i1 + i2)
							| _ -> raise (Failure (v1Type ^ " + " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Double (d1 +. (float_of_int i2))
							| Double(d2) -> Double (d1 +. d2)
							| _ -> raise (Failure (v1Type ^ " + " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Int(i2) -> Pitch (intToPitch(pitchToInt p1 + i2))
							| _ -> raise (Failure (v1Type ^ " + " ^ v2Type ^ " is not a valid operation")))
						| _ -> raise (Failure (v1Type ^ " + " ^ v2Type ^ " is not a valid operation")))
					(* v1 - v2 *)
					| Sub -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Double (float_of_int i1 -. d2)
							| Pitch(p2) -> Pitch (intToPitch(i1 - pitchToInt p2))
							| Int(i2) -> Int (i1 - i2)
							| _ -> raise (Failure (v1Type ^ " - " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Double (d1 -. float_of_int i2)
							| Double(d2) -> Double (d1 -. d2)
							| _ -> raise (Failure (v1Type ^ " - " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Int(i2) -> Pitch (intToPitch(pitchToInt p1 - i2))
							| _ -> raise (Failure (v1Type ^ " - " ^ v2Type ^ " is not a valid operation")))
						| _ -> raise (Failure (v1Type ^ " - " ^ v2Type ^ " is not a valid operation")))
					(* v1 * v2 *)
					| Mult -> (match v1 with
						Int(i1) -> (match v2 with
							Sound(p,d,a) -> Sound (p,float_of_int i1 *. d, a)
							| Double(d2) -> Double (float_of_int i1 *. d2)
							| Pitch(p2) -> Pitch (intToPitch(i1 * pitchToInt p2))
							| Int(i2) -> Int (i1 * i2)
							| _ -> raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Sound(p,d,a) -> Sound(p,d1 *. d, a)
							| Int(i2) -> Double (d1 *. float_of_int i2)
							| Double(d2) -> Double (d1 *. d2)
							| _ -> raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Int(i2) -> Pitch (intToPitch(pitchToInt p1 * i2))
							| _ -> raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
						| Sound(p,d,a) -> (match v2 with
							Int(i2) -> Sound(p,d *. float_of_int i2,a)
							| Double(d2) -> Sound(p,d *. d2,a)
							| _ -> raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
					  | _ ->raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
					(* v1 / v2 *)
					| Div -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Double (float_of_int i1 /. d2)
							| Pitch(p2) -> Pitch (intToPitch(i1 / pitchToInt p2))
							| Int(i2) -> Int (i1 / i2)
							| _ -> raise (Failure (v1Type ^ " / " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Double (d1 /. float_of_int i2)
							| Double(d2) -> Double (d1 /. d2)
							| _ -> raise (Failure (v1Type ^ " / " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Int(i2) -> Pitch (intToPitch(pitchToInt p1 / i2))
							| _ -> raise (Failure (v1Type ^ " / " ^ v2Type ^ " is not a valid operation")))
						| _ -> raise (Failure (v1Type ^ " / " ^ v2Type ^ " is not a valid operation")))
					(* v1 % v2 *)
					| Mod -> (match v1 with
						Int(i1) -> (match v2 with
							Int(i2) -> Int (i1 mod i2)
							| Pitch(p2) -> Pitch (intToPitch(i1 mod pitchToInt p2))
							| _ -> raise (Failure (v1Type ^ " % " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Int(i2) -> Pitch (intToPitch(pitchToInt p1 mod i2))
							| _ -> raise (Failure (v1Type ^ " % " ^ v2Type ^ " is not a valid operation")))
						| _ -> raise (Failure (v1Type ^ " % " ^ v2Type ^ " is not a valid operation")))
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
					| Eq -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Boolean (float_of_int i1 = d2)
							| Pitch(p2) -> Boolean (i1 = pitchToInt p2)
							| Int(i2) -> Boolean (i1 = i2)
							| _ -> raise (Failure (v1Type ^ " == " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Boolean (d1 = float_of_int i2)
							| Double(d2) -> Boolean (d1 = d2)
							| _ -> raise (Failure (v1Type ^ " == " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Pitch(p2) -> Boolean (pitchToInt p1 = pitchToInt p2)
							| Int(i2) -> Boolean (pitchToInt p1 = i2)
							| _ -> raise (Failure (v1Type ^ " == " ^ v2Type ^ " is not a valid operation")))
						| Boolean(b1) -> (match v2 with
							Boolean(b2) -> Boolean (b1 = b2)
							| _ -> raise (Failure (v1Type ^ " == " ^ v2Type ^ " is not a valid operation")))
						| _ -> raise (Failure (v1Type ^ " == " ^ v2Type ^ " is not a valid operation")))
					(* v1 != v2 *)
					| Neq -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Boolean (float_of_int i1 <> d2)
							| Pitch(p2) -> Boolean (i1 <> pitchToInt p2)
							| Int(i2) -> Boolean (i1 <> i2)
							| _ -> raise (Failure (v1Type ^ " != " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Boolean (d1 <> float_of_int i2)
							| Double(d2) -> Boolean (d1 <> d2)
							| _ -> raise (Failure (v1Type ^ " != " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Pitch(p2) -> Boolean (pitchToInt p1 <> pitchToInt p2)
							| Int(i2) -> Boolean (pitchToInt p1 <> i2)
							| _ -> raise (Failure (v1Type ^ " != " ^ v2Type ^ " is not a valid operation")))
						| Boolean(b1) -> (match v2 with
							Boolean(b2) -> Boolean (b1 <> b2)
							| _ -> raise (Failure (v1Type ^ " != " ^ v2Type ^ " is not a valid operation")))
						| _ -> raise (Failure (v1Type ^ " != " ^ v2Type ^ " is not a valid operation")))
					(* v1 < v2 *)
					| Lt -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Boolean (float_of_int i1 < d2)
							| Pitch(p2) -> Boolean (i1 < pitchToInt p2)
							| Int(i2) -> Boolean (i1 < i2)
							| _ -> raise (Failure (v1Type ^ " < " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Boolean (d1 < float_of_int i2)
							| Double(d2) -> Boolean (d1 < d2)
							| _ -> raise (Failure (v1Type ^ " < " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Pitch(p2) -> Boolean (pitchToInt p1 < pitchToInt p2)
							| Int(i2) -> Boolean (pitchToInt p1 < i2)
							| _ -> raise (Failure (v1Type ^ " < " ^ v2Type ^ " is not a valid operation")))
						| Boolean(b1) -> (match v2 with
							Boolean(b2) -> Boolean (b1 < b2)
							| _ -> raise (Failure (v1Type ^ " < " ^ v2Type ^ " is not a valid operation")))
						| _ -> raise (Failure (v1Type ^ " < " ^ v2Type ^ " is not a valid operation")))
					(* v1 > v2 *)
					| Gt -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Boolean (float_of_int i1 > d2)
							| Pitch(p2) -> Boolean (i1 > pitchToInt p2)
							| Int(i2) -> Boolean (i1 > i2)
							| _ -> raise (Failure (v1Type ^ " > " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Boolean (d1 > float_of_int i2)
							| Double(d2) -> Boolean (d1 > d2)
							| _ -> raise (Failure (v1Type ^ " > " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Pitch(p2) -> Boolean (pitchToInt p1 > pitchToInt p2)
							| Int(i2) -> Boolean (pitchToInt p1 > i2)
							| _ -> raise (Failure (v1Type ^ " > " ^ v2Type ^ " is not a valid operation")))
						| Boolean(b1) -> (match v2 with
							Boolean(b2) -> Boolean (b1 > b2)
							| _ -> raise (Failure (v1Type ^ " > " ^ v2Type ^ " is not a valid operation")))
						| _ -> raise (Failure (v1Type ^ " > " ^ v2Type ^ " is not a valid operation")))
					(* v1 <= v2 *)
					| Leq -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Boolean (float_of_int i1 <= d2)
							| Pitch(p2) -> Boolean (i1 <= pitchToInt p2)
							| Int(i2) -> Boolean (i1 <= i2)
							| _ -> raise (Failure (v1Type ^ " <= " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Boolean (d1 <= float_of_int i2)
							| Double(d2) -> Boolean (d1 <= d2)
							| _ -> raise (Failure (v1Type ^ " <= " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Pitch(p2) -> Boolean (pitchToInt p1 <= pitchToInt p2)
							| Int(i2) -> Boolean (pitchToInt p1 <= i2)
							| _ -> raise (Failure (v1Type ^ " <= " ^ v2Type ^ " is not a valid operation")))
						| Boolean(b1) -> (match v2 with
							Boolean(b2) -> Boolean (b1 <= b2)
							| _ -> raise (Failure (v1Type ^ " <= " ^ v2Type ^ " is not a valid operation")))
						| _ -> raise (Failure (v1Type ^ " <= " ^ v2Type ^ " is not a valid operation")))
					(* v1 >= v2 *)
					| Geq -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Boolean (float_of_int i1 >= d2)
							| Pitch(p2) -> Boolean (i1 >= pitchToInt p2)
							| Int(i2) -> Boolean (i1 >= i2)
							| _ -> raise (Failure (v1Type ^ " >= " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Boolean (d1 >= float_of_int i2)
							| Double(d2) -> Boolean (d1 >= d2)
							| _ -> raise (Failure (v1Type ^ " >= " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Pitch(p2) -> Boolean (pitchToInt p1 >= pitchToInt p2)
							| Int(i2) -> Boolean (pitchToInt p1 >= i2)
							| _ -> raise (Failure (v1Type ^ " >= " ^ v2Type ^ " is not a valid operation")))
						| Boolean(b1) -> (match v2 with
							Boolean(b2) -> Boolean (b1 >= b2)
							| _ -> raise (Failure (v1Type ^ " >= " ^ v2Type ^ " is not a valid operation")))
						| _ -> raise (Failure (v1Type ^ " >= " ^ v2Type ^ " is not a valid operation")))
					), env

				(* else raise (Failure ("Types " ^ v1Type ^ " and " ^ v2Type ^ " do not match")) *)

				(* !e *)
				| Not(e) ->
					let v, env = eval env e in
					let vType = getType v in
					(match v with
					Boolean(b) -> Boolean (not b), env
					| _ -> raise (Failure (vType ^ " has no ! operator")))

				(* -e *)
				| Neg(e) ->
					let v, env = eval env e in
					let vType = getType v in
					(match v with
					Int(i) -> Int (0 - i), env
					| Double(d) -> Double (0. -. d), env
					| _ -> raise (Failure (vType ^ " has no - operator")))
			
			(* Arrays *)
			| Array(e) -> 
				let evaledExprs, env = List.fold_left
					(fun (values, env) expr ->
						let v, env = eval env expr in v::values, env)
					([], env) (List.rev e)
				in
			 	Array(evaledExprs), env
			(* our special print function, only supports ints right now *)
			| Call("print", [e]) -> 
				let v, env = 
					(match eval env e with
						Index(a,i),_ -> eval env (Index(a,i))
						| _,_ -> eval env e) in
				let rec print = function
					Int(i) -> string_of_int i
					| Double(d) -> string_of_float d
					| Boolean(b) -> string_of_bool b
					| Pitch(p) -> p
					| Id(i) -> let v, _ = eval env (Id(i)) in
								print v
					| Sound(p,d,a) -> "|" ^ String.concat ", " p ^ "|:" ^ string_of_float d ^ ":" ^ string_of_int a
					| Array(a) -> "[" ^ build a ^ "]" and build = function
							hd :: [] -> (print hd)
							| hd :: tl -> ((print hd) ^ ", " ^ (build tl))
					| _ -> raise (Failure ("Item cannot be printed"))
				in
					print_endline (print v);
					Int(0), env

			| Call ("mixdown", actuals) ->
				let track_number = ref "0" in (*default track number if not specified*)
				let actuals, env = List.fold_left
					(fun (actuals, env) actual ->
						(* print_endline (Ast.string_of_expr actual); *)
					let v, env = eval env actual in v :: actuals, env)
					([], env) (List.rev actuals)
				in
				(* Check args see if track number is specified *)
				if List.length actuals = 2 then
					begin
						(* Checks if 2nd arg is an int and if it is within its range. Then sets track_number *)
						(try (int_of_string (Ast.string_of_expr (List.hd (List.tl actuals)))) with 
							Failure _ -> raise (Failure ("Invalid mixdown args. mixdown(<Array of Sounds or Sound>, <optional, Int, track_num, 0 - 15>")));
						track_number := (Ast.string_of_expr (List.hd (List.tl actuals)));
						if (((int_of_string !track_number) > 15) || ((int_of_string !track_number) < 0)) then 
							raise (Failure ("Invalid track_num in mixdown. track_num should be 0 - 15"))
					end;
				if List.length actuals > 2 then
					begin 
						raise (Failure ("Invalid mixdown args. mixdown(<Array of Sounds or Sound>, <optional Int trackNum>"))
					end;
				let file = "bytecode" in
				let rec writeByteCode = function
					Sound(p,d,a) -> "[" ^ String.concat ", " p ^ "]:" ^ string_of_float d ^ ":" ^ string_of_int a
					| Array(a) -> "[" ^ build a ^ "]" and build = function
							hd :: [] -> (writeByteCode hd)
							| hd :: tl -> ((writeByteCode hd) ^ "," ^ (build tl))
					| _ -> raise (Failure ("Item cannot be mixdown"))
				in 
					if !mixdown_flag = false then
						begin
							let oc = open_out file in
								fprintf oc "%s\n" (!track_number ^ (writeByteCode (List.hd actuals)));
								close_out oc
						end 
					else
						begin
							let oc = open_out_gen [Open_wronly; Open_creat; Open_append; Open_text] 0o666 file in
								output_string oc (!track_number ^ (writeByteCode (List.hd actuals)) ^ "\n");
								close_out oc
						end;
					print_endline ("Mixing down track " ^ !track_number);
					mixdown_flag := true;
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
				(*| Loop(v, a, s) -> 
                    let rec runloop env = function
                        [] -> env
                        | hd :: tl -> let var, env = eval env (Assign(v, hd)) in
							let env = exec env s in runloop env tl
                            in 
                            let arr, _ = eval env a in
                            (match arr with 
                                Array(x) -> runloop env x)
								*)
				| Loop(v, a, s) -> 
					let rec runloop env idx2 = function
						[] -> env
						| hd :: tl -> let idxlist = idx2::[] in
							let locals, globals = env in
							let env = 
								if NameMap.mem v locals then
									(NameMap.add v (Index(a,idxlist)) locals, globals)
								else if NameMap.mem v globals then
									(locals, NameMap.add v (Index(a,idxlist)) globals)
								else
									raise (Failure ("undeclared identifier " ^ v))
						    in
							let env = exec env s in runloop env (Int((getInt idx2)+1)) tl
					in 
					let arr, _ = eval env (Id(a)) in
					(match arr with 
						Array(x) -> runloop env (Int(0)) x)
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
