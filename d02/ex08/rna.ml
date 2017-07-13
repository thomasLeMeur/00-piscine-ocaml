#use "helix.ml" ;;

type rna = nucleobase list ;;

let generate_rna (lst : helix) =
	let rec convert final lst = match lst with
		| []	->	(final : rna)
		| f::e	->	let convert_nucl = function
						| A	-> U
						| T	-> A
						| C	-> G
						| G	-> C
						| _	-> None
					in
					convert (final@[convert_nucl f.n]) e
	in
	convert [] lst
;;
