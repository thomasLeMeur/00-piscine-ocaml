let rec leibniz_pi ?(count=0) ?(cur=0.) delta =
	if delta < 0. then
		-1
	else if abs_float ((4. *. cur) -. (4. *. (atan 1.))) <= delta then
		count
	else
		leibniz_pi ~count:(count + 1) ~cur:(cur +. (-1. *. (((mod_float (float_of_int count) 2.) -. 1.) *. 2. +. 1.)) /. (2. *. (float_of_int count) +. 1.)) delta
