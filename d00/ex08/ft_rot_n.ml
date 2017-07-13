let ft_rot_n n s =
	let f c =
		if not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) then
			c
		else
			let offset =
			if c >= 'a' && c <= 'z' then
				int_of_char 'a'
			else
				int_of_char 'A'
			in
			let chr = ((int_of_char c) - offset + n) mod 26 in
			char_of_int (offset + ((26 + chr) mod 26))
	in
	String.map (f) s
