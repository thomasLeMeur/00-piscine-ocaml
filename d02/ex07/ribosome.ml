#use "rna.ml" ;;

let generate_bases_triplets (lst : rna) =
	let rec generate final lst = match lst with
		| a::b::c::d	-> generate (final@[(a, b, c)]) d
		| _				-> final
	in
	generate [] lst
;;

type aminoacid = Stop | Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly | His | Ile | Leu | Lys | Met | Phe | Pro | Ser | Thr | Trp | Tyr | Val ;;
type protein = aminoacid list ;;

let rec string_of_protein (p : protein) = match p with
	| []	-> ""
	| b::e	->	let string_of_aminoacid = function
					| Stop	-> "End of translation"
					| Ala	-> "Alanine-"
					| Arg	-> "Arginine-"
					| Asn	-> "Asparagine-"
					| Asp	-> "Arpartique-"
					| Cys	-> "Cysteine-"
					| Gln	-> "Glutamine-"
					| Glu	-> "Glutamique-"
					| Gly	-> "Glycine-"
					| His	-> "Histidine-"
					| Ile	-> "Isoleucine-"
					| Leu	-> "Leucine-"
					| Lys	-> "Lysine-"
					| Met	-> "Methionine-"
					| Phe	-> "Phenylalanine-"
					| Pro	-> "Proline-"
					| Ser	-> "Serine-"
					| Thr	-> "Threonine-"
					| _		-> "|Unknown|"
				in
				(string_of_aminoacid b) ^ (string_of_protein e)
;;

let decode_arn (lst : rna) =
	let rec check1 a next =
		if a = (A, U, G) then
			(Met)::(loop next)
		else if a = (U, G, G) then
			(Trp)::(loop next)
		else
			loop next
	and check2 a b next =
		if a = (A, A, C) && b = (A, A, U) then
			(Asn)::(loop next)
		else if a = (G, A, C) && b = (G, A, U) then
			(Asp)::(loop next)
		else if a = (U, G, C) && b = (U, G, U) then
			(Cys)::(loop next)
		else if a = (C, A, A) && b = (C, A, G) then
			(Gln)::(loop next)
		else if a = (G, A, A) && b = (G, A, G) then
			(Glu)::(loop next)
		else if a = (C, A, C) && b = (C, A, U) then
			(His)::(loop next)
		else if a = (A, A, A) && b = (A, A, G) then
			(Lys)::(loop next)
		else if a = (U, U, C) && b = (U, U, U) then
			(Phe)::(loop next)
		else if a = (U, A, C) && b = (U, A, U) then
			(Tyr)::(loop next)
		else
			check1 a (b::next)
	and check3 a b c next =
		if a = (U, A, A) && b = (A, A, G) && c = (U, G, A) then
			[Stop]
		else if a = (A, U, A) && b = (A, U, C) && c = (A, U, U) then
			(Ile)::(loop next)
		else
			check2 a b (c::next)
	and check4 a b c d next =
		if a = (G, C, A) && b = (G, C, C) && c = (G, C, G) && d = (G, C, U) then
			(Ala)::(loop next)
		else if a = (G, G, A) && b = (G, G, C) && c = (G, G, G) && d = (G, G, U) then
			(Gly)::(loop next)
		else if a = (C, C, C) && b = (C, C, A) && c = (C, C, G) && d = (C, C, U) then
			(Pro)::(loop next)
		else if a = (A, C, A) && b = (A, C, C) && c = (A, C, G) && d = (A, C, U) then
			(Thr)::(loop next)
		else if a = (G, U, A) && b = (G, U, C) && c = (G, U, G) && d = (G, U, U) then
			(Val)::(loop next)
		else
			check3 a b c (d::next)
	and check6 a b c d e f next =
		if a = (A, G, A) && b = (A, G, G) && c = (C, G, A) && d = (C, G, C) && e = (C, G, G) && f = (C, G, U) then
			(Arg)::(loop next)
		else if a = (C, U, A) && b = (C, U, C) && c = (C, U, G) && d = (C, U, U) && e = (U, U, A) && f = (U, U, G) then
			(Leu)::(loop next)
		else if a = (U, C, A) && b = (U, C, C) && c = (U, C, G) && d = (U, C, U) && e = (A, G, U) && f = (A, G, C) then
			(Ser)::(loop next)
		else
			check4 a b c d (e::f::next)
	and loop = function
		| []					-> ([] : protein)
		| a::b::c::d::e::f::g	-> check6 a b c d e f g
		| a::b::c::d::e			-> check4 a b c d e
		| a::b::c::d			-> check3 a b c d
		| a::b::c				-> check2 a b c
		| a::b					-> check1 a b
	in
	loop (generate_bases_triplets lst)
;;
