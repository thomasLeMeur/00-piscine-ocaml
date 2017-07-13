module type MONOID =
sig
	type element
	val zero1 : element
	val zero2 : element
	val mul : element -> element -> element
	val add : element -> element -> element
	val div : element -> element -> element
	val sub : element -> element -> element
end

module INT : (MONOID with type element = int) =
struct
	type element = int
	let zero1 = 0
	let zero2 = 1
	let mul = ( * )
	let add = ( + )
	let div = ( / )
	let sub = ( - )
end

module FLOAT : (MONOID with type element = float) =
struct
	type element = float
	let zero1 = 1.0
	let zero2 = 1.0
	let mul = ( *. )
	let add = ( +. )
	let div = ( /. )
	let sub = ( -. )
end

module Calc =
	functor (M : MONOID) ->
	struct
		let add a b = M.add a b
		let sub a b = M.sub a b
		let mul a b = M.mul a b
		let div a b = M.div a b
		let rec power elem n =
			if n < 0 then M.zero2
			else if n == 0 then M.zero2
			else if n == 1 then elem
			else M.mul elem (power elem (n-1))
		let rec fact elem =
			if elem <= M.zero2 then M.zero2
			else M.mul elem (fact (M.sub elem M.zero1))
	end


module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)

let () =
	print_endline (string_of_int (Calc_int.power 3 3));
	print_endline (string_of_float (Calc_float.power 3.0 3));
	print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
	print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0))
