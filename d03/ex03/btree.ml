type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

(* Previous functions of previous exercises *)
let rec size = function
	| Nil			-> 0
	| Node (v, a, b)-> 1 + (size a) + (size b)

let height = function
	| Nil			-> 0
	| Node(v, a, b)	-> 1 + (max (size a) (size b))

let draw_tree btree =
	let rec draw_tree_node x y node = match node with
		| Nil			->	Graphics.moveto (x + 6) (y + 6);
							Graphics.draw_string "Nil";
							Graphics.draw_rect x y 30 26
		| Node (v, a, b)->	Graphics.moveto (x + 6) (y + 6);
							Graphics.draw_string v;
							let w = (String.length v) * 6 + 12 in
							Graphics.draw_rect x y w 26;

							let h = height node in
							Graphics.moveto (x + w) (y + 13);
							Graphics.lineto (x + w + 100) (y - 20 * h + 13);
							draw_tree_node (x + w + 100) (y - 20 * h) a;

							Graphics.moveto (x + w) (y + 13);
							Graphics.lineto (x + w + 100) (y + 20 * h + 13);
							draw_tree_node (x + w + 100) (y + 20 * h) b
	in
	draw_tree_node 10 ((Graphics.size_y ()) / 2) btree


(* Asked functions *)
let is_bst bst =
	let rec getValue ref node = match node with
		| Nil			-> ref
		| Node(v, a, b)	-> v
	and checkIsBst = function
		| Nil			->	true
		| Node(v, a, b)	->	if (getValue v a) > v || (getValue v b) < v then false
							else (checkIsBst a) && (checkIsBst b)
	in
	checkIsBst bst

let rec is_balanced = function
	| Nil			->	true
	| Node(v, a, b)	->	if (height a) <> (height b) then false
						else (is_balanced a) && (is_balanced b)

let rec is_perfect bst = match bst with
	| Nil			->	true
	| Node(v, a, b)	->	if (is_balanced bst) <> true || (a == Nil && b <> Nil) || (b == Nil && a <> Nil) then false
						else (is_perfect a) && (is_perfect b)

let rec search_bst value bst = match bst with
	| Nil			->	false
	| Node(v, a, b)	->	if v = value then true
						else (search_bst value a) || (search_bst value b)

let rec add_bst value bst = match bst with
	| Nil			->	Node(value, Nil, Nil)
	| Node(v, a, b)	->	if value <= v then Node(v, (add_bst value a), b)
						else Node(v, a, (add_bst value b))

let delete_bst bst value =
	let rec add_btree add bst = match add with
		| Nil			-> bst
		| Node(v, a, b)	-> add_btree b (add_btree a (add_bst v bst))
	and delete bst value = match bst with
		| Nil			->	Nil
		| Node(v, a, b)	->	if v <> value then Node(v, (delete a value), (delete b value))
						else add_btree (delete a value) (delete b value)
	in
	delete bst value

(*
let rec p v = match v with
	| true-> ()
	| false-> p (Graphics.key_pressed ())

let () =
	let empty = Nil in
	let bst = Node(4, Node(2, Node(1, Nil, Nil), Node(3, Nil, Nil)), Node(6, Node(5, Nil, Nil), Node(7, Nil, Nil))) in
	let treee = Node(4, Node(5, Nil, Nil), Nil) in

	print_endline "Arbre vide :";
	print_string "is_bst (true) : "; print_string (string_of_bool (is_bst empty)); print_char '\n';
	print_string "is_perfect (true) : "; print_string (string_of_bool (is_perfect empty)); print_char '\n';
	print_string "is_balanced (true) : "; print_string (string_of_bool (is_balanced empty)); print_char '\n';

	print_endline "Arbre bst :";
	print_string "is_bst (true) : "; print_string (string_of_bool (is_bst bst)); print_char '\n';
	print_string "is_perfect (true) : "; print_string (string_of_bool (is_perfect bst)); print_char '\n';
	print_string "is_balanced (true) : "; print_string (string_of_bool (is_balanced bst)); print_char '\n';

	print_endline "Arbre normal :";
	print_string "is_bst (false) : "; print_string (string_of_bool (is_bst treee)); print_char '\n';
	print_string "is_perfect (false) : "; print_string (string_of_bool (is_perfect treee)); print_char '\n';
	print_string "is_balanced (false) : "; print_string (string_of_bool (is_balanced treee)); print_char '\n';

	print_endline "Search in tree :";
	print_string "4 in treee (true) : "; print_string (string_of_bool (search_bst 4 treee)); print_char '\n';
	print_string "0 in treee (false) : "; print_string (string_of_bool (search_bst 0 treee)); print_char '\n';

	let treee2 = Node("4", Node("5", Nil, Nil), Nil) in
	print_string "Add 7 to treee2 : "; print_endline (string_of_bool (search_bst "7" (add_bst "7" treee2)));
	print_string "Remove 4 from treee2 : "; print_endline (string_of_bool (search_bst "4" (delete_bst treee2 "4")));
	let bst2 = Node("4", Node("2", Node("1", Nil, Nil), Node("3", Nil, Nil)), Node("6", Node("5", Nil, Nil), Node("7", Nil, Nil))) in
	print_string "Add 4 to bst2 : "; print_endline (string_of_bool (search_bst "4" (add_bst "4" bst2)));
	print_string "Remove 4 from bst2 : "; print_endline (string_of_bool (search_bst "4" (delete_bst (add_bst "4" bst2) "4")));
*)
