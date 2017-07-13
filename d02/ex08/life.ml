#use "ribosome.ml" ;;

let life =
	print_endline "Generating a random helix of 10000 elements";
	let hel = generate_helix 10000 in
	print_string "  -> "; print_endline (helix_to_string hel);
	print_endline "Generating the RNA of the helix";
	let r = generate_rna hel in
	print_endline "Generating the corresponding protein with its base triplets";
	let prot = decode_arn r in
	print_string "  -> "; print_endline (string_of_protein prot)
;;
