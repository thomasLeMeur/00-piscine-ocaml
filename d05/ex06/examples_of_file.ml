let rec addColumn lst finalLst last = match lst with
	| []		-> ((Array.of_list finalLst), last)
	| a::b::c	-> addColumn (b::c) (finalLst @ [(float_of_string a)]) last
	| a::b		-> addColumn b finalLst a

let rec addLine lst file =
	try
		let line = input_line file in
		addLine (lst @ [(addColumn (Str.split (Str.regexp ",") line) [] "")]) file
	with _ -> lst

let examples_of_file path =
	let file = open_in path in
	let lst = addLine [] file in
	close_in file;
	lst
