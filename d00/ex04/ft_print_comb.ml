let ft_print_comb () =
	let rec loopA a =
	if a <= 7 then
		let rec loopB b =
		if b <= 8 then
			let rec loopC c =
			if c <= 9 then
				begin
				if not (a == 0 && b == 1 && c == 2) then
					print_string ", ";
				print_int a; print_int b; print_int c;
				loopC (c + 1)
				end
			in
			begin
			loopC (b + 1);
			loopB (b + 1)
			end
		in
		begin
		loopB (a + 1);
		loopA (a + 1)
		end
	in
	loopA 0;
	print_string "\n"
