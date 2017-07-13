let rec printAllColors = function
	| []	-> print_char '\n'
	| hd::tl-> print_endline ((Color.toString hd) ^ " : " ^ (Color.toStringVerbose hd)); printAllColors tl

let () =
	printAllColors Color.all
