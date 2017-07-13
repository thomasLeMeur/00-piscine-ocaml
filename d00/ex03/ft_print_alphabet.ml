let ft_print_alphabet () =
	let ascii_a = int_of_char 'a' in
	let ascii_z = int_of_char 'z' in
	let rec loop ascii_code =
		if ascii_code <= ascii_z then
			begin
			print_char (char_of_int ascii_code);
			loop (ascii_code + 1)
			end
	in
	loop ascii_a;
	print_char '\n'
