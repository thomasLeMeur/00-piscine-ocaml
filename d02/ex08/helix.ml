#use "nucleotides.ml" ;;

Random.self_init() ;;

type helix = nucleotide list ;;

let rec generate_helix n = match n with
	| 0	->	([] : helix)
	| _	->	let random_nucl = function
				| 0	-> generate_nucleotide 'A'
				| 1	-> generate_nucleotide 'T'
				| 2	-> generate_nucleotide 'C'
				| 3	-> generate_nucleotide 'G'
				| _	-> generate_nucleotide '_'
			in
			(random_nucl (Random.int 4))::(generate_helix (n - 1))
;;

let helix_to_string (lst : helix) =
	let rec convert s lst = match lst with
		| []	->	s
		| f::e	->	let convert_nucl = function
						| A	-> "A"
						| T	-> "T"
						| C	-> "C"
						| G	-> "G"
						| _	-> "_"
					in
					convert (s ^ (convert_nucl f.n)) e
	in
	convert "" lst
;;

let complementary_helix (lst : helix) =
	let rec convert final lst = match lst with
		| []	->	(final : helix)
		| f::e	->	let convert_nucl = function
						| A	-> 'T'
						| T	-> 'A'
						| C	-> 'G'
						| G	-> 'C'
						| _	-> '_'
					in
					convert (final@[generate_nucleotide (convert_nucl f.n)]) e
	in
	convert [] lst
;;
