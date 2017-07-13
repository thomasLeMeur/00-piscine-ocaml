module type STRINGH =
sig
	type t = String.t
	val equal	: t -> t -> bool
	val hash	: t -> int
end

module StringH : STRINGH =
struct
	type t = String.t
	let equal a b = a == b
	
	let rec summarize sum s ind =
		if (String.length s) == ind then sum
		else summarize (int_of_char (String.get s ind)) s (ind + 1)
	let hash a = (summarize 0 a 0) / (String.length a)
end

module StringHashtbl : (Hashtbl.S with type key = StringH.t) = Hashtbl.Make (StringH)

let () =
	let ht = StringHashtbl.create 5 in
	let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
	let pairs = List.map (fun s -> (s, String.length s)) values in
	List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
	StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
