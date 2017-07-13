let rec converges f x n =
	if n < 0 then
		false
	else if n == 0 then
		if (f x) == x then
			true
		else
			false
	else
		converges f (f x) (n - 1)
