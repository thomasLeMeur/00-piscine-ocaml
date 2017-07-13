let rec printAllCards = function
	| []	-> ""
	| hd::tl-> hd ^ "\n" ^ (printAllCards tl)

let printCouple = function
	| (a, b) -> Deck.Card.toStringVerbose a

let () =
	let deck = Deck.newDeck () in
	print_endline (printAllCards (Deck.toStringList deck));
	print_endline (printAllCards (Deck.toStringListVerbose deck));
	print_endline (printCouple (Deck.drawCard deck))
