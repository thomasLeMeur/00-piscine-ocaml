let print_rev s =
	let rec loop ind =
	if ind >= 0 then
		begin
		print_char (String.get s ind);
		loop (ind - 1)
		end
	in
	loop ((String.length s) - 1);
	print_char '\n'

let main () =
	print_rev "Hello world !";
	print_rev ""

let () = main ()
