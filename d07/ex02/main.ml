let () =
	let d = new Doctor.doctor "Who" 42 "Robin" in
	print_endline d#to_string;
	d#talk;
	print_endline (d#travel_in 2017 2037)#to_string;
	d#use_sonic_screwdriver;
