let rec encode l =
	let rec count final nb elem lst = match lst with
		| []	-> final@[(nb, elem)]
		| a::b	-> if a == elem then (count final (nb + 1) elem b) else (count (final@[(nb, elem)]) 0 a lst)
	in
	let getFirst lst = match lst with
		| []	-> []
		| a::b	-> count [] 0 a lst
	in
	getFirst l
