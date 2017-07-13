let ft_string_all f s =
	let last = (String.length s) in
	let rec loop ind =
	if ind < last then
		(f (String.get s ind)) && loop (ind + 1)
	else
		true
	in
	loop 0
