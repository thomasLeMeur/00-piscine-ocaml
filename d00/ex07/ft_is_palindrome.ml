let ft_is_palindrome s =
	let rec loop first last =
		if first > last then
			true
		else
			if (String.get s first) == (String.get s last) then
				loop (first + 1) (last - 1)
			else
				false
	in
	loop 0 ((String.length s) - 1)
