let ft_power n p =
	if p == 0
	then
		1
	else
		let rec loop nb exp =
		if exp == 1
		then
			nb
		else
			(loop (nb * n) (exp - 1))
		in
		loop n p
