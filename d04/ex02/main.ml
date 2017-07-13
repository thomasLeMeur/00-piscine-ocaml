let rec printAllCards = function
	| []	-> print_char '\n'
	| hd::tl-> print_endline ((Card.toString hd) ^ " : " ^ (Card.toStringVerbose hd)); printAllCards tl

let () =
	printAllCards Card.all;

	Printf.printf "Value and color of 2S : (%s, %s)\n\n" (Card.Value.toString (Card.getValue (List.hd Card.allSpades))) (Card.Color.toString (Card.getColor (List.hd Card.allSpades)));

	Printf.printf "Compare 2S & 2D (0) : %d\n" (Card.compare (List.hd Card.allSpades) (List.hd Card.allDiamonds));
	Printf.printf "Compare 2S & 3D (-1) : %d\n" (Card.compare (List.hd Card.allSpades) (List.nth Card.allDiamonds 1));
	Printf.printf "Max 2S & 3D (3D) : %s\n" (Card.toString (Card.max (List.hd Card.allSpades) (List.nth Card.allDiamonds 1)));
	Printf.printf "Min 2S & 3D (2S) : %s\n" (Card.toString (Card.min (List.hd Card.allSpades) (List.nth Card.allDiamonds 1)));
	Printf.printf "Best of Spades (AS) : %s\n" (Card.toStringVerbose (Card.best Card.allSpades));
	Printf.printf "Best of Spades and 2S (2S) : %s\n\n" (Card.toStringVerbose (Card.best (Card.allSpades @ [(Card.newCard Card.Value.T2 Card.Color.Spade)])));

	Printf.printf "isSpade 2S (true) : %s\n" (string_of_bool (Card.isSpade (List.hd Card.allSpades)));
	Printf.printf "isHeart 2S (false) : %s\n" (string_of_bool (Card.isHeart (List.hd Card.allSpades)));
	Printf.printf "isDiamond 2D (true) : %s\n" (string_of_bool (Card.isDiamond (List.hd Card.allDiamonds)));
	Printf.printf "isClub 2S (false) : %s\n" (string_of_bool (Card.isClub (List.hd Card.allSpades)));
