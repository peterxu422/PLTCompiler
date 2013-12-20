open Ast open Printf open Helper

module NameMap = Map.Make(struct
	type t = string
	let compare x y = Pervasives.compare x y
end)

module TypeMap = Map.Make(struct
	type t = string
	let compare x y = Pervasives.compare x y
end)

let _ = Random.self_init()

(* Returns the type of an expression v *)
let getType v = 
	match v with
		Int(v) -> "int"
		| Double(v) -> "double"
		| Boolean(v) -> "bool"
		| Pitch(v) -> "pitch"
		| Sound(p,d,a) -> "sound"
		| Array(v::_) -> "array"
		| _ -> "unmatched_type"

(* Returns the evaluation of the boolean expression v *)
let getBoolean v =
	match v with
		Boolean(a) -> a
		| _ -> false

exception ReturnException of expr * expr NameMap.t

(* Sets the default initialization value for a given type t *)
let initType t = 
  match t with
    "int" -> Int(0)
    | "double" -> Double(0.0)
    | "bool" -> Boolean(false)
    | "pitch" -> Pitch("C0")
    | "sound" -> Sound((["C0"], 0., 0))
	| "intArr" -> Array([Int(0)])
	| "doubleArr" -> Array([Double(0.0)])
	| "booleanArr" -> Array([Boolean(false)])
	| "pitchArr" -> Array([Pitch("C0")])
	| "soundArr" -> Array([Sound(["C0"], 0., 0)])
	| _ -> Int(0)

(* if mixdown is not called, bytecode has x\n to indicate to BytecodeTranslator that it shouldn't attempt to play/write MIDI *)
let stopMixDown () = 
	let file = "bytecode" in
		let oc = open_out file in
			(fprintf oc "x\n";
			close_out oc)

(* global mixdown flag to see if mixdown has been called in which case we should append, not re write a file *)
let first_mixdown_flag = ref false;;
(* default bpm value *)
let bpm = ref 120;;
let opt = ref "b";; 

(* current function name *)
let fname = ref "" ;;

let run (vars, funcs) =
	(* Put function declarations in a symbol table *)
	let func_decls = List.fold_left
		(fun funcs fdecl -> NameMap.add fdecl.fname fdecl funcs)
		NameMap.empty funcs
	in
	(* Put function type in a table of function types *)
	let func_types = List.fold_left
		(fun funcs fdecl -> TypeMap.add fdecl.fname fdecl.rtype funcs)
		TypeMap.empty funcs
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
			(* Arrays *)
			| Array(e) -> 
				let evaledExprs, env = List.fold_left
					(fun (values, env) expr ->
						let v, env = eval env expr in v::values, env)
					([], env) (List.rev e)
				in
				(* type check *)
				(* make sure array isn't empty *)
				if evaledExprs = [] then (stopMixDown(); raise (Failure("Cannot initialize empty array")))
				else
				(* traverse through the array, make sure every element in the array is the same*)
				let hd = List.hd evaledExprs in
				let v1Type = getType hd in
				let rec check = function
					head::tail -> let v2Type = getType head in
								  if v1Type = v2Type then check tail
								  else (stopMixDown(); raise (Failure(v2Type^" in an array of type "^v1Type))) 
					| []	   -> evaledExprs
				in ignore(check evaledExprs);
			 	Array(evaledExprs), env
			(* Index *)
			| Index(a,i) -> let v, (locals, globals) = eval env (Id(a)) in
				let rec lookup arr indices =
					let arr = match arr with
						Array(n) -> n
						| _ -> (stopMixDown(); raise (Failure(a ^ " is not an array. Cannot access index")))
					in
					let index, env = eval env indices in 
					(match index with 
						Int(i) -> (try List.nth arr i, env
							with Failure("nth") -> stopMixDown(); raise (Failure "Index out of bounds"))
						| _ -> stopMixDown(); raise (Failure "Invalid index")
					)
				in
				lookup v (List.hd i)
			| Id(var) -> 
				let locals, globals = env in
				if NameMap.mem var locals then
					(NameMap.find var locals), env
				else if NameMap.mem var globals then
					(NameMap.find var globals), env
				else (stopMixDown(); raise (Failure ("undeclared identifier " ^ var)))
 			| Assign(var, e) ->
				(* for type check, use Index[0] as reference *)
				let lvar = (match var with
					Index(a, i) -> Index(a, [Int(0)])
					| _ -> var
				) in
				
				let v1, env = eval env lvar in
				let v1Type = getType v1 in
				
				let v, (locals, globals) = eval env e in
				(match var with
					Id(name) ->
					(match eval env (Id(name)) with
						Index(a, i), _ -> eval env (Assign((Index(a,i), e)))
						| _,_ ->
						
						let v2Type = getType v in
						(* The local identifiers have already been added to ST in the first pass. 
						Checks if it is indeed in there.*)
						if NameMap.mem name locals then	
							begin
								(* if both are arrays, check the type of its elements instead *)
								let v1Type2 = if v1Type = "array" && v2Type = "array" then
									(getType (match v1 with Array(v::_) -> v
															| _ -> v1))
								else v1Type in
								let v2Type2 = if v2Type = "array" && v1Type = "array" then
									(getType (match v with Array(d::_) -> d
															| _ -> v))
								else v2Type in
						
								(* Updates the var in the ST to evaluated expression e, which is stored in v. 
								Returns v as the value because this is the l-value*)
								if v1Type2 = v2Type2 then
									v, (NameMap.add name v locals, globals)
								else raise(Failure ("type mismatch: "^v1Type2^" with "^v2Type2))
							end
						else if NameMap.mem name globals then
							begin
								(* if both are arrays, check the type of its elements instead *)
								let v1Type2 = if v1Type = "array" && v2Type = "array" then
									(getType (match v1 with Array(v::_) -> v
															| _ -> v1))
								else v1Type in
								let v2Type2 = if v2Type = "array" && v1Type = "array" then
									(getType (match v with Array(d::_) -> d
															| _ -> v))
								else v2Type in
						
								(* Updates the var in the ST to evaluated expression e, which is stored in v. 
								Returns v as the value because this is the l-value*)
								if v1Type2 = v2Type2 then
									v, (locals, NameMap.add name v globals)
								else raise(Failure ("type mismatch: "^v1Type2^" with "^v2Type2))
							end
						else (stopMixDown(); raise (Failure ("undeclared identifier " ^ name)))
				)
				| Index(name, indices) -> 
					let rec getIndex e = 
						let v, env = eval env e in 
						(match v with
							Int(i) -> i
							(*Need to call getIndexFromVar again because function needs to return only 1 value*)
							| e ->
								print_endline (string_of_expr e);
							 	stopMixDown();
							 	raise (Failure ("Illegal index"))
					)
					in
					let v2Type = (getType v) in
					let rec setElt exprs = function
						[] -> (stopMixDown(); raise (Failure ("Cannot assign to empty array")))
						| hd :: [] -> let idx = getIndex hd in
							if idx < (List.length exprs) then
								let arr = (Array.of_list exprs) in arr.(idx) <- v; Array.to_list arr
							else
								let arr = 
								(Array.append 
									(Array.of_list exprs) 
									(Array.make (1+idx-(List.length exprs)) 
										(initType v2Type))) 
							in 
									arr.(idx) <- v; Array.to_list arr
						| _ -> (stopMixDown(); raise (Failure ("Cannot assign to this array")))
					in
					if NameMap.mem name locals then
						begin
							let exprList = (match (NameMap.find name locals) with
								Array(a) -> a
								| _ -> raise (Failure (name ^ " is not an array"))) in
							let newArray = Array(setElt exprList indices) in
							
							if v1Type = v2Type then
								v, (NameMap.add name newArray locals, globals)
							else (stopMixDown(); raise(Failure ("type mismatch: "^v1Type^" with "^v2Type)))
						end
					else if (NameMap.mem name globals) then
						begin
							let exprList = (match (NameMap.find name globals) with
								Array(a) -> a
								| _ -> (stopMixDown(); raise (Failure (name ^ " is not an array")))) in
							let newArray = Array(setElt exprList indices) in
	
							if v1Type = v2Type then
								v, (locals, NameMap.add name newArray globals)
							else (stopMixDown(); raise(Failure ("type mismatch: "^v1Type^" with "^v2Type)))
						end
					else
						(stopMixDown(); raise (Failure (name ^ " was not properly initialized as an array")))
			| _ -> (stopMixDown(); raise (Failure ("Can only assign variables or array indices"))))

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
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " + " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Double (d1 +. (float_of_int i2))
							| Double(d2) -> Double (d1 +. d2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " + " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Int(i2) -> Pitch (intToPitch(pitchToInt p1 + i2))
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " + " ^ v2Type ^ " is not a valid operation")))
						| _ -> stopMixDown(); raise (Failure (v1Type ^ " + " ^ v2Type ^ " is not a valid operation")))
					(* v1 - v2 *)
					| Sub -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Double (float_of_int i1 -. d2)
							| Pitch(p2) -> Pitch (intToPitch(i1 - pitchToInt p2))
							| Int(i2) -> Int (i1 - i2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " - " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Double (d1 -. float_of_int i2)
							| Double(d2) -> Double (d1 -. d2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " - " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Int(i2) -> Pitch (intToPitch(pitchToInt p1 - i2))
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " - " ^ v2Type ^ " is not a valid operation")))
						| _ -> stopMixDown(); raise (Failure (v1Type ^ " - " ^ v2Type ^ " is not a valid operation")))
					(* v1 * v2 *)
					| Mult ->
						(* Used for the array * int and int * array operations *)
						let rec buildList ls i =
							match i with
								1 -> ls
								| _ -> ls @ (buildList ls (i-1))
						in (match v1 with
						Int(i1) -> (match v2 with
							Array(a) -> Array (buildList a i1)
							| Sound(p,d,a) -> Sound (p,float_of_int i1 *. d, a)
							| Double(d2) -> Double (float_of_int i1 *. d2)
							| Pitch(p2) -> Pitch (intToPitch(i1 * pitchToInt p2))
							| Int(i2) -> Int (i1 * i2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Sound(p,d,a) -> Sound(p,d1 *. d, a)
							| Int(i2) -> Double (d1 *. float_of_int i2)
							| Double(d2) -> Double (d1 *. d2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Int(i2) -> Pitch (intToPitch(pitchToInt p1 * i2))
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
						| Sound(p,d,a) -> (match v2 with
							Int(i2) -> Sound(p,d *. float_of_int i2,a)
							| Double(d2) -> Sound(p,d *. d2,a)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
						| Array(a) -> (match v2 with
							Int(i2) -> Array (buildList a i2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
					  | _ -> stopMixDown(); raise (Failure (v1Type ^ " * " ^ v2Type ^ " is not a valid operation")))
					(* v1 / v2 *)
					| Div -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Double (float_of_int i1 /. d2)
							| Pitch(p2) -> Pitch (intToPitch(i1 / pitchToInt p2))
							| Int(i2) -> Int (i1 / i2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " / " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Double (d1 /. float_of_int i2)
							| Double(d2) -> Double (d1 /. d2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " / " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Int(i2) -> Pitch (intToPitch(pitchToInt p1 / i2))
							| _ -> raise (Failure (v1Type ^ " / " ^ v2Type ^ " is not a valid operation")))
						| _ -> stopMixDown(); raise (Failure (v1Type ^ " / " ^ v2Type ^ " is not a valid operation")))
					(* v1 % v2 *)
					| Mod -> (match v1 with
						Int(i1) -> (match v2 with
							Int(i2) -> Int (i1 mod i2)
							| Pitch(p2) -> Pitch (intToPitch(i1 mod pitchToInt p2))
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " % " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Int(i2) -> Pitch (intToPitch(pitchToInt p1 mod i2))
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " % " ^ v2Type ^ " is not a valid operation")))
						| _ -> stopMixDown(); raise (Failure (v1Type ^ " % " ^ v2Type ^ " is not a valid operation")))
					(* v1 || v2 *)
					| Or -> 
						if v1Type = "bool" && v2Type = "bool" then
							Boolean (getBoolean v1 || getBoolean v2)
						else (stopMixDown(); raise (Failure (v1Type ^ " || " ^ v2Type ^ " is not a valid operation")))
					(* v1 && v2 *)
					| And -> 
						if v1Type = "bool" && v2Type = "bool" then
							Boolean (getBoolean v1 && getBoolean v2)
						else (stopMixDown(); raise (Failure (v1Type ^ " && " ^ v2Type ^ " is not a valid operation")))
					(* v1 == v2 *)
					| Eq -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Boolean (float_of_int i1 = d2)
							| Pitch(p2) -> Boolean (i1 = pitchToInt p2)
							| Int(i2) -> Boolean (i1 = i2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " == " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Boolean (d1 = float_of_int i2)
							| Double(d2) -> Boolean (d1 = d2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " == " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Pitch(p2) -> Boolean (pitchToInt p1 = pitchToInt p2)
							| Int(i2) -> Boolean (pitchToInt p1 = i2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " == " ^ v2Type ^ " is not a valid operation")))
						| Boolean(b1) -> (match v2 with
							Boolean(b2) -> Boolean (b1 = b2)
							| _ -> raise (Failure (v1Type ^ " == " ^ v2Type ^ " is not a valid operation")))
						| _ -> stopMixDown(); raise (Failure (v1Type ^ " == " ^ v2Type ^ " is not a valid operation")))
					(* v1 != v2 *)
					| Neq -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Boolean (float_of_int i1 <> d2)
							| Pitch(p2) -> Boolean (i1 <> pitchToInt p2)
							| Int(i2) -> Boolean (i1 <> i2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " != " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Boolean (d1 <> float_of_int i2)
							| Double(d2) -> Boolean (d1 <> d2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " != " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Pitch(p2) -> Boolean (pitchToInt p1 <> pitchToInt p2)
							| Int(i2) -> Boolean (pitchToInt p1 <> i2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " != " ^ v2Type ^ " is not a valid operation")))
						| Boolean(b1) -> (match v2 with
							Boolean(b2) -> Boolean (b1 <> b2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " != " ^ v2Type ^ " is not a valid operation")))
						| _ -> stopMixDown(); raise (Failure (v1Type ^ " != " ^ v2Type ^ " is not a valid operation")))
					(* v1 < v2 *)
					| Lt -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Boolean (float_of_int i1 < d2)
							| Pitch(p2) -> Boolean (i1 < pitchToInt p2)
							| Int(i2) -> Boolean (i1 < i2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " < " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Boolean (d1 < float_of_int i2)
							| Double(d2) -> Boolean (d1 < d2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " < " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Pitch(p2) -> Boolean (pitchToInt p1 < pitchToInt p2)
							| Int(i2) -> Boolean (pitchToInt p1 < i2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " < " ^ v2Type ^ " is not a valid operation")))
						| Boolean(b1) -> (match v2 with
							Boolean(b2) -> Boolean (b1 < b2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " < " ^ v2Type ^ " is not a valid operation")))
						| _ -> stopMixDown(); raise (Failure (v1Type ^ " < " ^ v2Type ^ " is not a valid operation")))
					(* v1 > v2 *)
					| Gt -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Boolean (float_of_int i1 > d2)
							| Pitch(p2) -> Boolean (i1 > pitchToInt p2)
							| Int(i2) -> Boolean (i1 > i2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " > " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Boolean (d1 > float_of_int i2)
							| Double(d2) -> Boolean (d1 > d2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " > " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Pitch(p2) -> Boolean (pitchToInt p1 > pitchToInt p2)
							| Int(i2) -> Boolean (pitchToInt p1 > i2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " > " ^ v2Type ^ " is not a valid operation")))
						| Boolean(b1) -> (match v2 with
							Boolean(b2) -> Boolean (b1 > b2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " > " ^ v2Type ^ " is not a valid operation")))
						| _ -> stopMixDown(); raise (Failure (v1Type ^ " > " ^ v2Type ^ " is not a valid operation")))
					(* v1 <= v2 *)
					| Leq -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Boolean (float_of_int i1 <= d2)
							| Pitch(p2) -> Boolean (i1 <= pitchToInt p2)
							| Int(i2) -> Boolean (i1 <= i2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " <= " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Boolean (d1 <= float_of_int i2)
							| Double(d2) -> Boolean (d1 <= d2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " <= " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Pitch(p2) -> Boolean (pitchToInt p1 <= pitchToInt p2)
							| Int(i2) -> Boolean (pitchToInt p1 <= i2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " <= " ^ v2Type ^ " is not a valid operation")))
						| Boolean(b1) -> (match v2 with
							Boolean(b2) -> Boolean (b1 <= b2)
							| _ -> raise (Failure (v1Type ^ " <= " ^ v2Type ^ " is not a valid operation")))
						| _ -> stopMixDown(); raise (Failure (v1Type ^ " <= " ^ v2Type ^ " is not a valid operation")))
					(* v1 >= v2 *)
					| Geq -> (match v1 with
						Int(i1) -> (match v2 with
							Double(d2) -> Boolean (float_of_int i1 >= d2)
							| Pitch(p2) -> Boolean (i1 >= pitchToInt p2)
							| Int(i2) -> Boolean (i1 >= i2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " >= " ^ v2Type ^ " is not a valid operation")))
						| Double(d1) -> (match v2 with
							Int(i2) -> Boolean (d1 >= float_of_int i2)
							| Double(d2) -> Boolean (d1 >= d2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " >= " ^ v2Type ^ " is not a valid operation")))
						| Pitch(p1) -> (match v2 with
							Pitch(p2) -> Boolean (pitchToInt p1 >= pitchToInt p2)
							| Int(i2) -> Boolean (pitchToInt p1 >= i2)
							| _ -> stopMixDown(); raise (Failure (v1Type ^ " >= " ^ v2Type ^ " is not a valid operation")))
						| Boolean(b1) -> (match v2 with
							Boolean(b2) -> Boolean (b1 >= b2)
							| _ -> raise (Failure (v1Type ^ " >= " ^ v2Type ^ " is not a valid operation")))
						| _ -> stopMixDown(); raise (Failure (v1Type ^ " >= " ^ v2Type ^ " is not a valid operation")))
					), env

				(* !e *)
				| Not(e) ->
					let v, env = eval env e in
					let vType = getType v in
					(match v with
					Boolean(b) -> Boolean (not b), env
					| _ -> stopMixDown(); raise (Failure (vType ^ " has no ! operator")))

				(* -e *)
				| Neg(e) ->
					let v, env = eval env e in
					let vType = getType v in
					(match v with
					Int(i) -> Int (0 - i), env
					| Double(d) -> Double (0. -. d), env
					| _ -> stopMixDown(); raise (Failure (vType ^ " has no - operator")))
			
			| Call("setDuration", actuals) -> 
				let actuals, env = List.fold_left
					(fun (actuals, env) actual ->
					let v, env = eval env actual in v :: actuals, env)
					([], env) (List.rev actuals)
				in if (List.length actuals != 2)
				then (stopMixDown(); raise(Failure("setDuration takes a sound and a double")))
				else let newDuration = 
					(match (List.nth actuals 1) with
						Double(d) -> d
						| Index(a,i) -> 
							(match eval env (Index(a,i)) with
								Double(d),_ -> d
								| _,_ -> stopMixDown(); raise (Failure ("Second argument must evaluate to a double")) )
						| _ -> stopMixDown(); raise (Failure ("Second argument must evaluate to a double"))
					)
				in  
				(match (List.hd actuals) with
					Sound(p, d, a) -> Sound(p,newDuration,a), env
					| Index(a,i) -> 
						(match eval env (Index(a,i)) with
							Sound(p,d,a),_ -> Sound(p,newDuration,a), env
							| _,_ -> stopMixDown(); raise (Failure ("First argument must be a sound")) )
					| _ -> stopMixDown(); raise (Failure ("First argument must be a sound"))
				)
			| Call("setAmplitude", actuals) ->
				let actuals, env = List.fold_left
					(fun (actuals, env) actual ->
					let v, env = eval env actual in v :: actuals, env)
					([], env) (List.rev actuals)
				in if (List.length actuals != 2)
				then (stopMixDown(); raise(Failure("setAmplitude takes a sound and an integer")))
				else let newAmplitude = 
					(match (List.nth actuals 1) with
						Int(i) -> i
						| Index(a,i) -> 
							(match eval env (Index(a,i)) with
								Int(i),_ -> i
								| _,_ -> stopMixDown(); raise (Failure ("Second argument must evaluate to an integer")) )
						| _ -> stopMixDown(); raise (Failure ("Second argument must evaluate to an integer"))
					)
				in  
				(match (List.hd actuals) with
					Sound(p, d, a) -> Sound(p,d,newAmplitude), env
					| Index(a,i) -> 
						(match eval env (Index(a,i)) with
							Sound(p,d,a),_ -> Sound(p,d,newAmplitude), env
							| _,_ -> stopMixDown(); raise (Failure ("First argument must be a sound")) )
					| _ -> stopMixDown(); raise (Failure ("First argument must be a sound"))
				)
			| Call("setPitches", actuals) ->
				let actuals, env = List.fold_left
					(fun (actuals, env) actual ->
					let v, env = eval env actual in v :: actuals, env)
					([], env) (List.rev actuals)
				in if (List.length actuals != 2)
				then raise(Failure("setPitches takes a sound and a pitch array"))
				else let newPitches = 
					(match (List.nth actuals 1) with
						Array(i) -> (* print_endline (string_of_expr (List.hd i)); *)
							List.rev (List.map (fun e -> string_of_expr e) i)
						| Index(a,i) -> 
							(match eval env (Index(a,i)) with
								Array(i),_ -> List.rev (List.map (fun e -> string_of_expr e) i)
								| Pitch(p),_ -> p::[]
								| _,_ -> stopMixDown(); raise (Failure ("Second argument must be an array of pitches")) )
						| _ -> stopMixDown(); raise (Failure ("Second argument must be an array of pitches"))
					)
				in  
				(match (List.hd actuals) with
					Sound(p, d, a) -> Sound(newPitches,d,a), env
					| Index(a,i) -> 
						(match eval env (Index(a,i)) with
							Sound(p,d,a),_ -> Sound(newPitches,d,a), env
							| _,_ -> stopMixDown(); raise (Failure ("First argument must be a sound")) )
					| _ -> stopMixDown(); raise (Failure ("First argument must be a sound"))
				)
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
					| Sound(p,d,a) -> "|" ^ String.concat ", " (List.rev p) ^ "|:" ^ string_of_float d ^ ":" ^ string_of_int a
					| Array(a) -> let rec build = function
							hd :: [] -> (print hd)
							| hd :: tl -> ((print hd) ^ ", " ^ (build tl))
							| _ -> stopMixDown(); raise (Failure ("Item cannot be printed"))
						in
						"[" ^ build a ^ "]"
					| _ -> stopMixDown(); raise (Failure ("Item cannot be printed"))
				in
					print_endline (print v);
					Int(0), env

			| Call ("mixDown", actuals) ->
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
						ignore(try (int_of_string (Ast.string_of_expr (List.hd (List.tl actuals)))) with 
							Failure _ -> raise (stopMixDown(); Failure ("Invalid mixdown args. mixdown(<Array of Sounds or Sound>, <optional, Int, track_num, 0 - 15>")));
						track_number := (Ast.string_of_expr (List.hd (List.tl actuals)));
						if (((int_of_string !track_number) > 15) || ((int_of_string !track_number) < 0)) then 
							(stopMixDown(); raise (Failure ("Invalid track_num in mixdown. track_num should be 0 - 15")))
					end;
				if List.length actuals > 2 then
					begin
						stopMixDown();
						raise (Failure ("Invalid mixdown args. mixdown(<Array of Sounds or Sound>, <optional Int trackNum>"))
					end;
				let file = "bytecode" in
				let rec writeByteCode = function
					Sound(p,d,a) -> "[" ^ String.concat ", " p ^ "]:" ^ string_of_float d ^ ":" ^ string_of_int a
					| Array(a) -> let rec build = function
							hd :: [] -> (writeByteCode hd)
							| hd :: tl -> ((writeByteCode hd) ^ "," ^ (build tl))
							| _  -> raise (Failure ("invalid array format"))
						in 
						"[" ^ build a ^ "]" 
					| _ -> raise (stopMixDown(); Failure ("argument cannot be mixdown"))
				in 
					if !first_mixdown_flag = false then
						begin
							let oc = open_out file in
								fprintf oc "%s %s\n" (string_of_int !bpm) !opt;
								fprintf oc "%s\n" (!track_number ^ (writeByteCode (List.hd actuals)));
								close_out oc
						end 
					else
						begin
							let oc = open_out_gen [Open_wronly; Open_creat; Open_append; Open_text] 0o666 file in
								output_string oc (!track_number ^ (writeByteCode (List.hd actuals)) ^ "\n");
								close_out oc
						end;
					(* print_endline ("Mixing down track " ^ !track_number); *)
					first_mixdown_flag := true;
					Int(0), env
			(* for pitches and sounds *)
			| Call("getAmplitude", [e]) ->
				let v, env = eval env e in
				(match v with
					  Sound(p,d,a) -> Int(a), env
					  | Index(a,i) -> 
						(match eval env (Index(a,i)) with
							Sound(p,d,a),_ -> Int(a), env
							| _,_ -> stopMixDown(); raise (Failure ("getAmplitude can only be called on sounds")) )
					| _ -> stopMixDown(); raise (Failure ("getAmplitude can only be called on sounds"))
				)
			(* for sounds *)
			| Call("getDuration", [e]) ->
				let v, env = eval env e in
				(match v with
					  Sound(p,d,a) -> Double(d), env
					  | Index(a,i) -> 
						(match eval env (Index(a,i)) with
							Sound(p,d,a),_ -> Double(d), env
							| _,_ -> stopMixDown(); raise (Failure ("getDuration can only be called on sounds")) )
					| _ -> stopMixDown(); raise (Failure ("getDuration can only be called on sounds"))
				)
			(* for pitches and sounds *)
			| Call("getPitches", [e]) ->
				let v, env = eval env e in
				(match v with
					  Sound(p,d,a) -> 
					  let rec strings_to_pitches = function
					  		  hd :: [] -> [Pitch(hd)]
					  		| hd :: tl -> [Pitch(hd)] @ strings_to_pitches tl
					  		| _ -> stopMixDown(); raise (Failure ("getPitches can only be called on sounds with pitches"))
					  in
					  Array(List.rev(strings_to_pitches p)), env
					| Pitch(p) -> Pitch(p), env
					| Index(a,i) -> 
						(match eval env (Index(a,i)) with
							Sound(p,d,a),_ -> 
							let rec strings_to_pitches = function
								  hd :: [] -> [Pitch(hd)]
								| hd :: tl -> [Pitch(hd)] @ strings_to_pitches tl
								| _ -> stopMixDown(); raise (Failure ("getPitches can only be called on sounds with pitches"))
							 in
							 Array(List.rev(strings_to_pitches p)), env
							| _,_ -> stopMixDown(); raise (Failure ("getPitches can only be called on sounds or pitches")) )
					| _ -> stopMixDown(); raise (Failure ("getPitches can only be called on sounds or pitches"))
				)
			| Call("randomInt", [bound]) -> 
				let v, env = eval env bound in
				(match v with
					  Int(i) -> Int(Random.int i), env
					| _ -> stopMixDown(); raise (Failure ("argument must be an int"))
				)
			| Call("randomDouble", [bound]) ->
				let v, env = eval env bound in
				(match v with
					  Double(d) -> Double(Random.float d), env
					| _ -> stopMixDown(); raise (Failure ("argument must be a double"))
				)
			(* for arrays eyes only *)
			| Call("length", [e]) -> 
				let v, env = eval env e in
				(match v with
					  Array(a) -> Int(List.length a), env
					| _ -> stopMixDown(); raise (Failure ("Length can only be called on arrays"))
				)
			(* sets the bpm *)
			| Call("bpm", actuals) -> 
				let actuals, env = List.fold_left
					(fun (actuals, env) actual ->
					let v, env = eval env actual in v :: actuals, env)
					([], env) (List.rev actuals)
				in
					if !first_mixdown_flag != false then (stopMixDown(); raise (Failure ("Bpm must be set before mixdown is called to take affect")));
					if List.length actuals != 1 then (stopMixDown(); raise (Failure ("One argument must be passed to bpm")));
					ignore(try (int_of_string (Ast.string_of_expr (List.hd actuals))) with
						Failure _ -> stopMixDown(); raise (Failure ("int must be passed to bpm. 40 - 300 is suggested")));
					bpm := (int_of_string (Ast.string_of_expr (List.hd actuals)));
					Int(0), env
			(* midi is only written and not played *)
			| Call("write", []) ->
				opt := "w";
				Int(0), env
			(* midi is only played and not written *)
			| Call("play", []) ->
				opt := "p";
				Int(0), env
			(* this does function calls. *)
			| Call(f, actuals) -> 
				fname := f;
				let fdecl =
				  try NameMap.find f func_decls
				  with Not_found -> stopMixDown(); raise (Failure ("undefined function " ^ f))
				in
				(* get function type for initializing function return value *)
				let ftype =
				  try TypeMap.find f func_types
				  with Not_found -> stopMixDown(); raise (Failure ("undefined function " ^f))
				in
				let actuals, env = List.fold_left
					(fun (actuals, env) actual ->
					let v, env = eval env actual in v :: actuals, env)
					([], env) (List.rev actuals)
				in
				let (locals, globals) = env in
				try
					let globals = call fdecl actuals globals
					in (initType ftype), (locals, globals)
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
									(stopMixDown(); raise (Failure ("undeclared identifier " ^ v)))
						    in
							let env = exec env s in match idx2 with
								Int(i) -> runloop env (Int(i+1)) tl
								| _ -> runloop env (Int(0+1)) tl
					in 
					let arr, _ = eval env (Id(a)) in
					(match arr with 
						Array(x) -> runloop env (Int(0)) x
						| _ -> stopMixDown(); raise (Failure ("Looping on array was expected")))

				| Return(e) ->
				let v, (locals, globals) = eval env e in
				(* find current function type *)
				let ftype1 =
				  try TypeMap.find !fname func_types
				  with Not_found -> stopMixDown(); raise (Failure ("undefined function " ^(!fname)))
				in
				let ftype = (match ftype1 with
								 "intArr"		-> "array"
								|"doubleArr"	-> "array"
								|"booleanArr"	-> "array"
								|"pitchArr"		-> "array"
								|"soundArr"		-> "array"
								|"boolean"		-> "bool"
								| _				-> ftype1)
				in
				let farrtype = (match ftype1 with
								 "intArr"		-> "int"
								|"doubleArr"	-> "double"
								|"booleanArr"	-> "bool"
								|"pitchArr"		-> "pitch"
								|"soundArr"		-> "sound"
								|"boolean"		-> "bool"
								| _				-> ftype1)
				in

				let rtype = (getType v) in
				
				(* if both are arrays, check the type of its elements instead *)
				let ftype2 = if ftype = "array" && rtype = "array"
					then
						farrtype
					else ftype
				in
				let rtype2 = if ftype = "array" && rtype = "array" then
					(getType (match v with Array(d::_) -> d
												| _ -> v))
					else rtype
				in
				(* check function type and return type *)
				if ftype2 <> rtype2 then
					(stopMixDown(); raise(Failure("function type is of "^ftype2^" while the return type is of "^rtype2)))
				;
				raise (ReturnException(v, globals))
			in  
			(* Enter the function: bind actual values to formal arguments *)
			let locals = 
				try List.fold_left2
					(fun locals formal actual -> NameMap.add formal.paramname actual locals) NameMap.empty fdecl.formals actuals
				with Invalid_argument(_) ->
					(stopMixDown(); raise (Failure ("wrong number of arguments to: " ^ fdecl.fname)))
			in
			let locals = List.fold_left (* init locals to 0 *)
				(fun locals local -> if (NameMap.mem local.varname locals)
									 then
										raise (Failure (local.varname^" has already been declared locally"))
									 else
										NameMap.add local.varname (initType local.vartype) locals) locals fdecl.locals
			in
	    	snd (List.fold_left exec (locals, globals) fdecl.body)

		in let globals = List.fold_left
			(fun globals vdecl -> if (NameMap.mem vdecl.varname globals)
								  then
									  raise (Failure (vdecl.varname^" has already been dclared globally"))
								  else
									  NameMap.add vdecl.varname (initType vdecl.vartype) globals) NameMap.empty vars
		in try
			call (NameMap.find "main" func_decls) [] globals
		with Not_found -> (stopMixDown(); raise (Failure("did not find the main() function")))

let _ = 
	let lexbuf = Lexing.from_channel stdin in 
	let program = Parser.program Scanner.token lexbuf in
	stopMixDown();
	run program
