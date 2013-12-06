module NameMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

module IntMap = Map.Make(struct
  type t = int
  let compare x y = Pervasives.compare x y
end)

let maxPitchInt = 119;;
let minPitchInt = 0;;

let pitchToIntMap = NameMap.empty;;
(* Cb ???????*)
(* let pitchToIntMap = NameMap.add "Cb" 0 pitchToIntMap;;   *)
let pitchToIntMap = NameMap.add "C" 0 pitchToIntMap;;  
let pitchToIntMap = NameMap.add "C#" 1 pitchToIntMap;; 
let pitchToIntMap = NameMap.add "Db" 1 pitchToIntMap;; 
let pitchToIntMap = NameMap.add "D" 2 pitchToIntMap;; 
let pitchToIntMap = NameMap.add "D#" 3 pitchToIntMap;; 
let pitchToIntMap = NameMap.add "Eb" 3 pitchToIntMap;; 
let pitchToIntMap = NameMap.add "E" 4 pitchToIntMap;; 
let pitchToIntMap = NameMap.add "E#" 5 pitchToIntMap;; 
let pitchToIntMap = NameMap.add "Fb" 4 pitchToIntMap;; 
let pitchToIntMap = NameMap.add "F" 5 pitchToIntMap;; 
let pitchToIntMap = NameMap.add "F#" 6 pitchToIntMap;; 
let pitchToIntMap = NameMap.add "Gb" 6 pitchToIntMap;; 
let pitchToIntMap = NameMap.add "G" 7 pitchToIntMap;; 
let pitchToIntMap = NameMap.add "G#" 8 pitchToIntMap;; 
let pitchToIntMap = NameMap.add "Ab" 8 pitchToIntMap;; 
let pitchToIntMap = NameMap.add "A" 9 pitchToIntMap;;
let pitchToIntMap = NameMap.add "A#" 10 pitchToIntMap;; 
let pitchToIntMap = NameMap.add "Bb" 10 pitchToIntMap;; 
let pitchToIntMap = NameMap.add "B" 11 pitchToIntMap;; 
(* B# ???????? *)
(* let pitchToIntMap = NameMap.add "B#" 12 pitchToIntMap;;  *)

let intToPitchMap = IntMap.empty;;
let intToPitchMap = IntMap.add 0 "C" intToPitchMap;;  
let intToPitchMap = IntMap.add 1 "C#" intToPitchMap;; 
let intToPitchMap = IntMap.add 2 "D" intToPitchMap;; 
let intToPitchMap = IntMap.add 3 "Eb" intToPitchMap;; 
let intToPitchMap = IntMap.add 4 "E" intToPitchMap;; 
let intToPitchMap = IntMap.add 5 "F" intToPitchMap;; 
let intToPitchMap = IntMap.add 6 "F#" intToPitchMap;; 
let intToPitchMap = IntMap.add 7 "G" intToPitchMap;; 
let intToPitchMap = IntMap.add 8 "G#" intToPitchMap;; 
let intToPitchMap = IntMap.add 9 "A" intToPitchMap;; 
let intToPitchMap = IntMap.add 10 "Bb" intToPitchMap;; 
let intToPitchMap = IntMap.add 11 "B" intToPitchMap;;

let pitchToInt = fun x ->
	let octave = String.get x ((String.length x)-1) in
		let basicPitch = String.sub x 0 ((String.length x)-1) in
			((NameMap.find basicPitch pitchToIntMap) + ((int_of_char octave) - 48) * 12)
(* There is nothing here to see if you are out of range *)

let intToPitch = fun x -> 
	if x > maxPitchInt then raise (Failure ("Pitch higher than allowable threshold"))
		else if x < minPitchInt then raise (Failure ("Pitch lower than allowable threshold"))
		else (IntMap.find (x - 12*(x / 12)) intToPitchMap) ^ (string_of_int (x / 12))