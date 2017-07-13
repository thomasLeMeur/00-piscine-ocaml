let gray n =
	if n <= 0 then
		print_int 0
	else
		let rec max_nb acc cur = match cur with
			| 0	-> acc
			| _	-> max_nb (acc * 2) (cur - 1)
		and print_bits nb cur = match cur with
			| tmp when tmp == n	-> ()
			| _					-> print_int (if nb land (1 lsl (n - (cur + 1))) != 0 then 1 else 0); print_bits nb (cur + 1)
		and pprint nb =
			if nb != 0 then
				print_char ' ';
			print_bits (nb lxor (nb lsr 1)) 0
		and count cur max = match cur with
			| x when x == max	-> ()
			| _					-> pprint cur; count (cur + 1) max
		in
		count 0 (max_nb 1 n);
	print_char '\n'
