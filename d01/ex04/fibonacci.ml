let rec fibonacci ?(n_2=0) ?(n_1=1) n =
	if n < 0 then
		-1
	else if n == 0 then
		n_2
	else if n == 1 then
		n_1
	else
		fibonacci ~n_2:n_1 ~n_1:(n_2 + n_1) (n - 1)
