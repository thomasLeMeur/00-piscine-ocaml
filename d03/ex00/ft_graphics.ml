type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square x y size =
	let offset = size / 2 in
	Graphics.moveto (x - offset) (y + offset);
	Graphics.lineto (x + offset) (y + offset);
	Graphics.lineto (x + offset) (y - offset);
	Graphics.lineto (x - offset) (y - offset);
	Graphics.lineto (x - offset) (y + offset)

let draw_tree_node btree =
	let rec loop x y node = match node with
		| Nil			->	Graphics.moveto (x - 7) (y - 6);
							Graphics.draw_string "Nil";
							draw_square x y 50
		| Node(v, a, b)	->	Graphics.moveto (x - 14) (y - 6);
							Graphics.draw_string "value";
							draw_square x y 50;

							Graphics.moveto (x + 25) y;
							Graphics.lineto (x + 125) (y - 50);
							loop (x + 150) (y - 50) a;

							Graphics.moveto (x + 25) y;
							Graphics.lineto (x + 125) (y + 50);
							loop (x + 150) (y + 50) b
	in
	loop 600 300 btree

(*
let rec p v = match v with
	| true-> ()
	| false-> p (Graphics.key_pressed ())

let () =
	Graphics.open_graph " 1280x720";
	draw_tree_node (Node(42, Nil, Nil));
	p (Graphics.key_pressed ())
*)
