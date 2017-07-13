let rec ft_sum expr ?(total=0.) lower upper =
	if upper < lower then
		nan
	else if lower == upper then
		total +. (expr lower)
	else
		ft_sum expr ~total:(total +. (expr lower)) (lower + 1) upper
