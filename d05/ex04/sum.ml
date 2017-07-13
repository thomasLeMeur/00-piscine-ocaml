let sum a b = a +. b

let () =
	print_string "1. + 2. = ";
	Printf.printf "%f\n" (sum 1. 2.);
	(*
	print_string "1 + 2 = ";
	try Printf.printf "%f\n" (sum 1 2);
	with _ -> print_endline "an error occured";
	*)
	(*
	print_string "'a' + 'b' = ";
	try Printf.printf "%f\n" (sum 'a' 'b');
	with _ -> print_endline "an error occured";
	*)
