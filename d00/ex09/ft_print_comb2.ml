let ft_print_comb2 () =
	let rec loopA a =
	if a <= 99 then
		let rec loopB b =
		if b <= 99 then
			begin
			if not (a == 0 && b == 1) then
				print_char ',';
			if not (a == 0 && b == 1) then
				print_char ' ';
			print_int (a / 10); print_int (a mod 10);
			print_char ' ';
			print_int (b / 10); print_int (b mod 10);
			loopB (b + 1)
			end
		in
		begin
		loopB (a + 1);
		loopA (a + 1)
		end
	in
	loopA 0;
	print_char '\n'
