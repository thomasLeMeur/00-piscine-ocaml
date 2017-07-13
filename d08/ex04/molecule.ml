class virtual molecule (name : string) (atoms : Atom.atom list) =
object (self)
	val _atoms  = atoms

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
		computeFinalformula (computeAllItems _atoms [])

	method get_atoms = _atoms

	method name = name
	method formula = self#compute_formula

	method to_string = self#name ^ " (" ^ self#formula ^ ")"
	method equals (other : molecule) = (self#name = other#name) && (self#formula = other#formula)
end

class dioxygen =
object
	inherit molecule "Dioxygen"
	[
	new Atom.oxygen; new Atom.oxygen
	]
end

class water =
object
	inherit molecule "Water"
	[
	new Atom.oxygen;
	new Atom.hydrogen; new Atom.hydrogen
	]
end

class carbonDioxyde =
object
	inherit molecule "Carbon dioxyde"
	[
	new Atom.carbon;
	new Atom.oxygen; new Atom.oxygen
	]
end

class aspirine =
object
	inherit molecule "Aspirin"
	[
	new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen;
	new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
		new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
	new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon;
		new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon;
	new Atom.carbon
	]
end

class caffeine =
object
	inherit molecule "Caffeine"
	[
	new Atom.oxygen; new Atom.oxygen; new Atom.oxygen;
	new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
		new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
		new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
	new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon;
		new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon;
	new Atom.nitrogen; new Atom.nitrogen; new Atom.nitrogen; new Atom.nitrogen
	]
end

class ecstasy =
object
	inherit molecule "Ecstasy"
	[
	new Atom.oxygen; new Atom.oxygen;
	new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
		new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
		new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
		new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
	new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon;
		new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon;
		new Atom.carbon; new Atom.carbon; new Atom.carbon;
	new Atom.nitrogen
	]
end

class piperazinomycinMonohydrobromide =
object
	inherit molecule "Piperazinomycin monohydrobromide"
	[
	new Atom.oxygen; new Atom.oxygen;
	new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
		new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
		new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
		new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
		new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
		new Atom.hydrogen;
	new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon;
		new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon;
		new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon;
		new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon;
		new Atom.carbon; new Atom.carbon;
	new Atom.nitrogen; new Atom.nitrogen;
	new Atom.bromine
	]
end

(*
let () =
	let w = new water in
	let cd = new carbonDioxyde in
	let a = new aspirine in
	let c = new caffeine in
	let e = new ecstasy in
	let p = new piperazinomycinMonohydrobromide in
	print_endline w#to_string;
	print_endline cd#to_string;
	print_endline a#to_string;
	print_endline c#to_string;
	print_endline e#to_string;
	print_endline p#to_string;
*)
