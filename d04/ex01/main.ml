let rec printAllColors = function
	| []	-> print_char '\n'
	| hd::tl-> print_endline ((Color.toString hd) ^ " : " ^ (Color.toStringVerbose hd)); printAllColors tl

let rec printAllValues = function
	| []	-> print_char '\n'
	| hd::tl-> print_endline ((Value.toString hd) ^ " : " ^ (Value.toStringVerbose hd)); printAllValues tl

let rec loopOnNextValue v = match v with
	| Value.As	-> print_endline "end"
	| _			-> loopOnNextValue (Value.next v)

let rec loopOnPreviousValue v = match v with
	| Value.T2	-> print_endline "end"
	| _			-> loopOnPreviousValue (Value.previous v)

let () =
	printAllColors Color.all;
	printAllValues Value.all;
	loopOnNextValue Value.T2;
	loopOnPreviousValue Value.As
