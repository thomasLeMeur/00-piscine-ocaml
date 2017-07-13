let my_sleep () = Unix.sleep 1

let () =
	try
		let n = int_of_string (Sys.argv.(1)) in 
		for i = 1 to n do
			my_sleep ()
		done;
	with _ -> ()
