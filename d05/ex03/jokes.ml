let findJokesNb () =
	let file = open_in Sys.argv.(1) in
	let i = ref 0 in
	try
		while true do
			ignore (input_line file);
			incr i;
		done;
		close_in file;
		!i
	with _ -> ();
	close_in file;
	!i

let f ind = 
	let file = open_in Sys.argv.(1) in
	for i = 1 to ind do
		ignore (input_line file);
	done;
	let s = input_line file in
	close_in file;
	s

let () =
	Random.self_init ();
	try
		let arr = Array.init (findJokesNb ()) f in
		print_endline (arr.(Random.int (Array.length arr)))
	with _ -> Printf.printf "Usage : %s file_to_read\n" Sys.argv.(0)
