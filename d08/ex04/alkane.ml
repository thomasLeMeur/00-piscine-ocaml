class virtual alkane (n : int) =
object (self)
	method get_atoms = self#get_alkane_atoms n

	method name = self#get_alkane_name n
	method formula = self#compute_formula

	method to_string = self#name ^ " (" ^ self#formula ^ ")"
	method equals (other : alkane) = (self#name = other#name) && (self#formula = other#formula)

	method private get_alkane_name = function
		| 1		-> "Methane"
		| 2		-> "Ethane"
		| 3		-> "Propane"
		| 4		-> "Butane"
		| 5		-> "Pentane"
		| 6		-> "Hexane"
		| 7		-> "Heptane"
		| 8		-> "Octane"
		| 9		-> "Nonane"
		| 10	-> "Decane"
		| 11	-> "Undecane"
		| 12	-> "Dodecane"
		| 13	-> "Tridecane"
		| 14	-> "Tetradecane"
		| 15	-> "Pentadecane"
		| 16	-> "Cetane"
		| _		-> "Infane"

	method private get_alkane_atoms ?(final=[]) n =
		if n < 0 then final
		else if n == 0 then final @ [new Atom.hydrogen; new Atom.hydrogen]
		else self#get_alkane_atoms ~final:(final @ [new Atom.carbon; new Atom.hydrogen; new Atom.hydrogen]) (n-1)

	method private compute_formula =
			let rec insertItem it lst final = match lst with
				| []	->	final @ [(it, 1)]
				| hd::tl->	if it = "C" || (it = "H" && (fst hd) <> "C") then
								if fst hd = it then final @ [(it, (snd hd) + 1)] @ tl
								else final @ [(it, 1)] @ [hd] @ tl
							else if fst hd < it then insertItem it tl (final @ [hd])
							else if fst hd = it then final @ [(it, (snd hd) + 1)] @ tl
							else if fst hd <> "C" && fst hd <> "H" then final @ [(it, 1)] @ [hd] @ tl
							else insertItem it tl (final @ [hd])
			in
			let rec computeAllItems lst final = match lst with
				| []	-> final
				| hd::tl-> computeAllItems tl (insertItem hd#symbol final [])
			in
			let rec computeFinalformula = function
				| []	-> ""
				| hd::tl-> (fst hd) ^ (if (snd hd) > 1 then (string_of_int (snd hd)) else "") ^ (computeFinalformula tl)
			in
			computeFinalformula (computeAllItems self#get_atoms [])
end

class methane = object inherit alkane 1 end
class ethane = object inherit alkane 2 end
class propane = object inherit alkane 3 end
class butane = object inherit alkane 4 end
class pentane = object inherit alkane 5 end
class hexane = object inherit alkane 6 end
class heptane = object inherit alkane 7 end
class octane = object inherit alkane 8 end
class nonane = object inherit alkane 9 end
class decane = object inherit alkane 10 end
class undecane = object inherit alkane 11 end
class dodecane = object inherit alkane 12 end
class tridecane = object inherit alkane 13 end
class tetradecane = object inherit alkane 14 end
class pentadecane = object inherit alkane 15 end
class cetane = object inherit alkane 16 end

(*
let () = 
	let a1 = new methane in
	let a2 = new ethane in
	let a3 = new propane in
	let a4 = new butane in
	let a5 = new pentane in
	let a6 = new hexane in
	let a7 = new heptane in
	let a8 = new octane in
	let a9 = new nonane in
	let a10 = new decane in
	let a11 = new undecane in
	let a12 = new dodecane in
	let a13 = new tridecane in
	let a14 = new tetradecane in
	let a15 = new pentadecane in
	let a16 = new cetane in
	print_endline a1#to_string;
	print_endline a2#to_string;
	print_endline a3#to_string;
	print_endline a4#to_string;
	print_endline a5#to_string;
	print_endline a6#to_string;
	print_endline a7#to_string;
	print_endline a8#to_string;
	print_endline a9#to_string;
	print_endline a10#to_string;
	print_endline a11#to_string;
	print_endline a12#to_string;
	print_endline a13#to_string;
	print_endline a14#to_string;
	print_endline a15#to_string;
	print_endline a16#to_string;
*)
