let eu_dist a b =
	let sum = ref 0. in
	let lim = (Array.length a) - 1 in
	for i = 0 to lim do
		let diff = a.(i) -. b.(i) in
		sum := !sum +. (diff *. diff);
	done;
	sqrt !sum

let f1 ind = float_of_int (ind + 1)
let f2 ind = float_of_int (ind + 8)

let () =
	Printf.printf "%f\n" (eu_dist (Array.init 4 f1) (Array.init 4 f2))
