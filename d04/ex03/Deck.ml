module Card =
	struct
	module Color =
		struct
		type t	= Spade | Heart | Diamond | Club

		let all	= [Spade; Heart; Diamond; Club]

		let toString = function
			| Spade		-> "S"
			| Heart		-> "H"
			| Diamond	-> "D"
			| Club		-> "C"

		let toStringVerbose = function
			| Spade		-> "Spade"
			| Heart		-> "Heart"
			| Diamond	-> "Diamond"
			| Club		-> "Club"
		end

	module Value =
		struct
		type t	= T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

		let all	= [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

		let toInt = function
			| T2	-> 1
			| T3	-> 2
			| T4	-> 3
			| T5	-> 4
			| T6	-> 5
			| T7	-> 6
			| T8	-> 7
			| T9	-> 8
			| T10	-> 9
			| Jack	-> 10
			| Queen	-> 11
			| King	-> 12
			| As	-> 13

		let toString = function
			| T2	-> "2"
			| T3	-> "3"
			| T4	-> "4"
			| T5	-> "5"
			| T6	-> "6"
			| T7	-> "7"
			| T8	-> "8"
			| T9	-> "9"
			| T10	-> "10"
			| Jack	-> "J"
			| Queen	-> "Q"
			| King	-> "K"
			| As	-> "A"

		let toStringVerbose = function
			| T2	-> "2"
			| T3	-> "3"
			| T4	-> "4"
			| T5	-> "5"
			| T6	-> "6"
			| T7	-> "7"
			| T8	-> "8"
			| T9	-> "9"
			| T10	-> "10"
			| Jack	-> "Jack"
			| Queen	-> "Queen"
			| King	-> "King"
			| As	-> "As"

		let next = function
			| T2	-> T3
			| T3	-> T4
			| T4	-> T5
			| T5	-> T6
			| T6	-> T7
			| T7	-> T8
			| T8	-> T9
			| T9	-> T10
			| T10	-> Jack
			| Jack	-> Queen
			| Queen	-> King
			| King	-> As
			| As	-> invalid_arg "As is the last card value, there is no next one"

		let previous = function
			| T2	-> invalid_arg "T2 is the first card value, there is no previous one"
			| T3	-> T2
			| T4	-> T3
			| T5	-> T4
			| T6	-> T5
			| T7	-> T6
			| T8	-> T7
			| T9	-> T8
			| T10	-> T9
			| Jack	-> T10
			| Queen	-> Jack
			| King	-> Queen
			| As	-> King
		end

	type t = Value.t * Color.t

	let newCard v c	= ( v, c )

	let allSpades	= List.map (function v -> newCard v Color.Spade) Value.all
	let allHearts	= List.map (function v -> newCard v Color.Heart) Value.all
	let allDiamonds	= List.map (function v -> newCard v Color.Diamond) Value.all
	let allClubs	= List.map (function v -> newCard v Color.Club) Value.all
	let all			= allSpades @ allHearts @ allDiamonds @ allClubs

	let getValue (v, _)	= v
	let getColor (_, c)	= c

	let toString (v, c)			= (Value.toString v) ^ (Color.toString c)
	let toStringVerbose (v, c)	= Printf.sprintf "Card(%s, %s)" (Value.toStringVerbose v) (Color.toStringVerbose c)

	let compare (v1, c1) (v2, c2)	= (Value.toInt v1) - (Value.toInt v2)
	let max a b						= if (compare a b) >= 0 then a else b
	let min a b						= if (compare a b) <= 0 then a else b
	let best = function
		| []	-> invalid_arg "To find the best, the list has to be not empty"
		| hd::tl-> List.fold_left (fun a b -> if (compare a b) == 0 then a else b) hd tl

	let isOf (v, c) ref	= c == ref
	let isSpade c		= isOf c Color.Spade
	let isHeart c		= isOf c Color.Heart
	let isDiamond c		= isOf c Color.Diamond
	let isClub c		= isOf c Color.Club
	end

type t = Card.t list

let newDeck () = Random.self_init (); List.stable_sort (fun a b -> ((Random.int 3) - 1)) Card.all

let toStringList deck =
	let rec doLoop curDeck lst = match curDeck with
		| []	-> lst
		| hd::tl-> doLoop tl (lst @ [(Card.toString hd)])
	in
	doLoop deck []
let toStringListVerbose deck =
	let rec doLoop curDeck lst = match curDeck with
		| []	-> lst
		| hd::tl-> doLoop tl (lst @ [(Card.toStringVerbose hd)])
	in
	doLoop deck []

let drawCard = function
	| []	-> raise (Failure "To draw a card, the deck has to be not empty")
	| hd::tl-> (hd, tl)
