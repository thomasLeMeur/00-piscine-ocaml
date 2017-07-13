type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec size = function
	| Nil			-> 0
	| Node (v, a, b)-> 1 + (size a) + (size b)

let rec height = function
	| Nil			-> 0
	| Node (v, a, b)-> 1 + (max (size a) (size b))

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

(*
let rec p v = match v with
	| true	-> ()
	| false	-> p (Graphics.key_pressed ())

let () =
	Graphics.open_graph " 1280x720";
	let a = (Node("3", Node("0", Node("1", Nil, Nil), Node("2", Nil, Nil)), Node("4", Node("5", Nil, Nil), Node("6", Nil, Nil)))) in
	draw_tree (Node("3", a, a));
	p (Graphics.key_pressed ())
*)
