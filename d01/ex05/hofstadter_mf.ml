let rec hfs_f n =
	if n < 0 then
		-1
	else if n == 0 then
		1
	else
		n - (hfs_m (hfs_f (n - 1)))

and hfs_m n =
	if n < 0 then
		-1
	else if n == 0 then
		0
	else
		n - (hfs_f (hfs_m (n - 1)))
