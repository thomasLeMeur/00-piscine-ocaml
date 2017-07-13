let sequence n =
	if n < 0 then
		""
	else
		let rec toString s lst = match lst with
			| []	-> s
			| a::b	-> toString (s ^ (string_of_int a)) b
		and wrap final lst = match lst with
			| []	-> final
			| a::b	-> let (n, v) = a in wrap (final@[n;v]) b
		and count final nb elem lst = match lst with
			| []-> final@[(nb, elem)]
			| a::b-> if a == elem then (count final (nb + 1) elem b) else (count (final@[(nb, elem)]) 0 a lst)
		and getFirst lst = match lst with
			| []-> []
			| a::b-> count [] 0 a lst
		and encode cur lst =
			if cur == n then
				lst
			else
				encode (cur + 1) (wrap [] (getFirst lst))
		in
		toString "" (encode 0 [1])
