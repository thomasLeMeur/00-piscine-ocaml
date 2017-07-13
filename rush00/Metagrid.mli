module Player :
sig
	type t = X | O | Nil | None
end

module Grid :
sig
	module Case :
	sig
		type t

		val newEmptyCase: unit -> t
		val newCase		: Player.t -> t

		val getOwner: t -> Player.t
		val getRepr	: t -> string
	end

	type t

	val newEmptyGrid : unit -> t
	val newGrid : Case.t list -> Player.t -> t

	val isEmptyCase	: int -> int -> t -> bool
	val setCaseOwner: int -> int -> Player.t -> t -> t

	val getOwner: t -> Player.t
	val getRepr	: t -> string list
end

type t

val newEmptyMetaGrid : unit -> t
val newMetaGrid : Grid.t list -> Player.t -> t

val getGrids : t -> Grid.t list

val isEmptyCase	: int -> int -> t -> bool
val setCaseOwner: int -> int -> Player.t -> t -> t

val getOwner: t -> Player.t
val getRepr	: t -> string
val isLost	: t -> bool
