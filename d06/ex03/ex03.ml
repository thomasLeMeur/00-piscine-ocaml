module type FRACTIONNAL_BITS =
sig
	val bits : int
end

module type FIXED =
sig
	type t
	val of_float : float -> t
	val of_int : int -> t
	val to_float : t -> float
	val to_int : t -> int
	val to_string : t -> string
	val zero : t
	val one : t
	val succ : t -> t
	val pred : t -> t
	val min : t -> t -> t
	val max : t -> t -> t
	val gth : t -> t -> bool
	val lth : t -> t -> bool
	val gte : t -> t -> bool
	val lte : t -> t -> bool
	val eqp : t -> t -> bool (** physical equality *)
	val eqs : t -> t -> bool (** structural equality *)
	val add : t -> t -> t
	val sub : t -> t -> t
	val mul : t -> t -> t
	val div : t -> t -> t
	val foreach : t -> t -> (t -> unit) -> unit
end

module type MAKE =
	functor (Bits : FRACTIONNAL_BITS) ->
	FIXED

module Make : MAKE =
	functor (Bits : FRACTIONNAL_BITS) ->
	struct
		type t = int

		let of_float f =
			let tmp = f *. (2.0 ** (float_of_int Bits.bits)) in
			if (ceil tmp) -. tmp <= tmp -. (floor tmp) then int_of_float (ceil tmp)
			else int_of_float (floor tmp)
		let of_int i = of_float (float_of_int i)

		let to_float v = (float_of_int v) /. (2.0 ** (float_of_int Bits.bits))
		let to_int v = int_of_float (to_float v)
		let to_string v = string_of_float (to_float v)
		
		let zero = of_float 0.0
		let one = of_float 1.0
		
		let succ v = v + 1
		let pred v = v - 1

		let gth a b = (to_float a) > (to_float b)
		let lth a b = (to_float a) < (to_float b)
		let gte a b = (to_float a) >= (to_float b)
		let lte a b = (to_float a) <= (to_float b)
		let eqp a b = (to_float a) = (to_float b)
		let eqs a b = (to_float a) == (to_float b)

		let min a b = if (lte a b) == true then a else b
		let max a b = if (gte a b) == true then a else b
		
		let add a b = of_float ((to_float a) +. (to_float b))
		let sub a b = of_float ((to_float a) -. (to_float b))
		let mul a b = of_float ((to_float a) *. (to_float b))
		let div a b = of_float ((to_float a) /. (to_float b))

		let rec foreach cur lim f = 
			if cur > lim then ()
			else (f cur; foreach (succ cur) lim f)
	end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
	let x8 = Fixed8.of_float 21.10 in
	let y8 = Fixed8.of_float 21.32 in
	let r8 = Fixed8.add x8 y8 in
	print_endline (Fixed8.to_string r8);
	Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))
