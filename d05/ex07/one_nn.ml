let eu_dist a b =
	let sum = ref 0. in
	let lim = (Array.length a) - 1 in
	for i = 0 to lim do
		let diff = a.(i) -. b.(i) in
		sum := !sum +. (diff *. diff);
	done;
	sqrt !sum

let one_nn radars (refL, refS) =
	let mini = ref (-1.0) in
	let state = ref refS in
	let rec loop = function
		| []	-> ()
		| hd::tl->
				begin
				match hd with (l, s) ->
					let d = eu_dist refL l in
					let diff = abs_float (d -. !mini) in
					if !mini < 0.0 || diff < !mini then
						mini := diff;
						state := s;
					loop tl
				end
	in
	loop radars;
	!state
