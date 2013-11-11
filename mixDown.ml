(* In lullabyte, MixDown takes in a dynamic array of a dynamic array
 * of sounds.
 * In OCaml, I am writing it to take in a list of lists of sounds (or strings?).
 * mixDown will use List.iter to output each array on a separate line *)
let mixDown arrayOfArrays =

	(* breakDownTrack will output the sounds of an array separated by spaces *)
	let breakDownTrack ary =

		(* printPitch prints a pitch, duration, and amplitude in the format
		 * specified
		 * Confusion: i'm writing this function like pitch, duration, and
		 * amplitude are strings, but are they at this point in OCaml?
		 * Even if they are, how would we write getPitches, getDuration,
		 * getAmplitude? I'm looking at string functions now *)
		let printPitch pitch duration amplitude =
			print_endline (pitch ^ "/" ^ duration "a" ^ amplitude ^ "+") in

		(* breakDownSound will output the sound broken down into sounds of
   	 * individual pitches.
   	 * The sounds will be separated by +s *)
		let breakDownSound sd =

			(* I have no idea how to write these functions because I don't know
			 * if a sound is a string in OCaml or a tuple.
			 * I'm not sure where they would be in our compilation process *)
			let pitches = getPitches sd in
			let duration = getDuration sd in
			let amplitude = getAmplitude sd in

			List.iter print_pitch pitches duration amplitude in (* breakDownSound *)

		List.iter breakDownSound sd in (* breakDownTrack *)

	List.iter breakDownTrack arrayOfArrays