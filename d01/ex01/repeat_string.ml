let rec repeat_string ?(str="x") n =
	if n < 0 then
		"Error"
	else if n == 0 then
		""
	else
		str ^ (repeat_string ~str:str (n - 1))
