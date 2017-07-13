let rot42 s =
	let f c =
		if not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) then
			c
		else
			let offset =
			if c >= 'a' && c <= 'z' then int_of_char 'a'
			else int_of_char 'A'
			in
			let chr = ((int_of_char c) - offset + 42) mod 26 in
			char_of_int (offset + ((26 + chr) mod 26))
	in
	String.map f s

let caesar s key =
	let f c =
		if not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) then
			c
		else
			let offset =
			if c >= 'a' && c <= 'z' then int_of_char 'a'
			else int_of_char 'A'
			in
			let chr = ((int_of_char c) - offset + key) mod 26 in
			char_of_int (offset + ((26 + chr) mod 26))
	in
	String.map f s

let xor s key =
	let f c =
		char_of_int ((int_of_char c) lxor key)
	in
	String.map f s

let rec ft_crypt s lst = match lst with
	| []	-> s
	| hd::tl	-> ft_crypt (hd s 42) tl
