class alkane_combustion (startM : (Molecule.molecule * int) list) (endM : (Molecule.molecule * int) list) =
object (self)
	inherit Reaction.reaction startM endM as super

	method get_start =
		if not self#is_balanced then invalid_arg "The reaction is not balanced"
		else startM

	method get_result =
		if not self#is_balanced then invalid_arg "The reaction is not balanced"
		else endM

	method balance = new alkane_combustion (startM @ [(new Molecule.dioxygen, 1)]) (endM @ [(new Molecule.water, 1)])

	method is_balanced =
		let diff = self#findAtomsOfDiff (self#concatAllAtomsOfManyMolecules startM []) (self#concatAllAtomsOfManyMolecules endM []) in
		diff = []

	method private insertItemOrAddIt it lst final = match lst with
		| []	->	final @ [(it, 1)]
		| hd::tl->	if it = "C" || (it = "H" && (fst hd) <> "C") then
						if fst hd = it then final @ [(it, (snd hd) + 1)] @ tl
						else final @ [(it, 1); hd] @ tl
					else if fst hd < it then self#insertItemOrAddIt it tl (final @ [hd])
					else if fst hd = it then final @ [(it, (snd hd) + 1)] @ tl
					else if fst hd <> "C" && fst hd <> "H" then final @ [(it, 1); hd] @ tl
					else self#insertItemOrAddIt it tl (final @ [hd])

	method private concatAllAtomsOfManyMolecules lst final = match lst with
			| []	->	final
			| hd::tl->	let rec concatAllAtoms lst final = match lst with
							| []-> final
							| hd::tl-> concatAllAtoms tl (self#insertItemOrAddIt hd#symbol final [])
						in
						let rec concatAllMolecules n mol final =
							if n <= 0 then final
							else concatAllMolecules (n-1) mol (final @ mol#get_atoms)
						in
						self#concatAllAtomsOfManyMolecules tl (final @ (concatAllAtoms (concatAllMolecules (snd hd) (fst hd) []) []))

	method private findAtomsOfDiff listA listB =
		let rec updateNbOfOneAtom lst it n final = match lst with
			| []	->	final @ [(it, n)]
			| hd::tl->	if it <> (fst hd) then updateNbOfOneAtom tl it n (final @ [hd])
						else final @ [(it, ((snd hd) - n))]
		in
		let rec updateNbOfAtoms left right = match right with
			| []	-> left
			| hd::tl-> updateNbOfAtoms (updateNbOfOneAtom left (fst hd) (snd hd) []) tl
		in
		let rec deleteEmptyAtoms lst final = match lst with
			| []	->	final
			| hd::tl->	if snd hd <> 0 then deleteEmptyAtoms tl (final @ [hd])
						else deleteEmptyAtoms tl final
		in
		deleteEmptyAtoms (updateNbOfAtoms listA listB) []

end

let () =
	let combB = new alkane_combustion [(new Alkane.methane, 1); (new Molecule.dioxygen, 2)] [(new Molecule.carbonDioxyde, 1); (new Molecule.water, 2)] in
	let combNB = new alkane_combustion [(new Alkane.methane, 1); (new Molecule.dioxygen, 1)] [(new Molecule.carbonDioxyde, 1); (new Molecule.water, 1)] in
	Printf.printf "combB is balanced (true) : %s\n" (string_of_bool combB#is_balanced);
	Printf.printf "combNB is balanced (false) : %s\n" (string_of_bool combNB#is_balanced);
	Printf.printf "Balanced combNB is balanced (true) : %s\n" (string_of_bool (combNB#balance)#is_balanced);
