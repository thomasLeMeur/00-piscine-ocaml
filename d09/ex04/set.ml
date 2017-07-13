module type SET =
sig
	type 'a t
	val return : 'a -> 'a t
	val bind : 'a t -> ('a -> 'b t) -> 'b t
	val union : 'a t -> 'a t -> 'a t
	val inter : 'a t -> 'a t -> 'a t
	val diff : 'a t -> 'a t -> 'a t
	val filter : 'a t -> ('a -> bool) -> 'a t
	val foreach : 'a t -> ('a -> unit) -> unit
	val for_all : 'a t -> ('a -> bool) -> bool
	val exists : 'a t -> ('a -> bool) -> bool
end

module Set : SET =
struct
	type 'a t = 'a list
	let return v = [v]
	let rec bind v f = match v with
		| []	->	[]
		| hd::tl->	(f hd) @ (bind tl f)
	let union a b = a @ b

	let inter a b =
		let rec isIn ref lst = match lst with
			| []	-> false
			| hd::tl-> (ref = hd) || (isIn ref tl)
		and perform l1 l2 = match l1 with
			| []	-> []
			| hd::tl-> (if (isIn hd l2) then [hd] else []) @ (perform tl l2)
		in
		perform a b
	let diff a b =
		let rec isIn ref lst = match lst with
			| []	-> false
			| hd::tl-> (ref = hd) || (isIn ref tl)
		and perform l1 l2 = match l1 with
			| []	-> []
			| hd::tl-> (if not (isIn hd l2) then [hd] else []) @ (perform tl l2)
		in
		(perform a b) @ (perform b a)
	let rec filter v f = match v with
		| []	->	[]
		| hd::tl->	(if (f hd) then [hd] else []) @ (filter tl f)
	let rec foreach v f = match v with
		| []	->	()
		| hd::tl->	f hd; foreach tl f
	let rec for_all v f = match v with
		| []	->	true
		| hd::tl->	(f hd) && (for_all tl f)
	let rec exists v f = match v with
		| []	->	false
		| hd::tl->	(f hd) || (exists tl f)
end
