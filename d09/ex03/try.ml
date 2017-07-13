module type TRY =
sig
	type 'a t = Success of 'a | Failure of exn
	val return : 'a -> 'a t
	val bind : 'a t -> ('a -> 'b t) -> 'b t
	val recover : 'a t -> (exn -> 'a t) -> 'a t
	val filter : 'a t -> ('a -> bool) -> 'a t
	val flatten : 'a t t -> 'a t
end

module Try : TRY =
struct
	type 'a t = Success of 'a | Failure of exn
	let return v = Success (v)
	let bind v f = match v with
		| Failure (_)	-> Failure(Invalid_argument "The binding is impossible on Failure()")
		| Success (a)	-> f a
	let recover v f = match v with
		| Failure (e)	-> f e
		| Success (a)	-> v
	let filter v f = match v with
		| Failure (_)	-> v
		| Success (a)	-> if not (f a) then Failure(Invalid_argument "The filter is not true") else v
	let flatten = function
		| Success (a)	-> a
		| Failure (e)	-> Failure(e)
end
