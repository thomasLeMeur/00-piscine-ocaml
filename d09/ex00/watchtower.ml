module type WATCHTOWER =
sig
	type hour = int
	val zero : hour
	val add : hour -> hour -> hour
	val sub : hour -> hour -> hour
end

module Watchtower : WATCHTOWER =
struct
	type hour = int

	let zero = 0
	let add a b = (a + b) mod 12
	let sub a b = (12 + (a - b)) mod 12
end
