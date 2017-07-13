let () =
	let p = new People.people "Who" in
	print_endline p#to_string;
	p#talk;
	p#die
