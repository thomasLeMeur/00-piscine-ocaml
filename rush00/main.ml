type input = { y : int; x : int }

let rec getInput map =
	let s = read_line () in
	try
		let inputs = List.map int_of_string (Str.split (Str.regexp " ") s) in
		if (List.length inputs) <> 2 then
			begin
			print_endline "Incorrect format.";
			getInput map
			end
		else
			let coords = { y = ((List.nth inputs 0) - 1); x = ((List.nth inputs 1) - 1) } in
			if (Metagrid.isEmptyCase coords.x coords.y map) = false then
				begin
				print_endline "Illegal move.";
				getInput map
				end
			else coords
	with _ -> print_endline "Incorrect format."; getInput map

let rec gameLoop indOfTurn p1 p2 map =
	if (Metagrid.isLost map) = true then
		begin
		Printf.printf "Everybody loose!\n\n%s" (Metagrid.getRepr map);
		getPlay ()
		end
	else
		let mainOwner = Metagrid.getOwner map in
		if mainOwner = Metagrid.Player.O then
			begin
			Printf.printf "%s win the game!\n\n%s" p1 (Metagrid.getRepr map);
			getPlay ()
			end
		else if mainOwner = Metagrid.Player.X then
			begin
			Printf.printf "%s win the game!\n\n%s" p2 (Metagrid.getRepr map);
			getPlay ()
			end
		else
			print_endline (Metagrid.getRepr map);
			let curPlayer = if indOfTurn mod 2 = 0 then p1 else p2 in
			Printf.printf "%s's turn to play.\n" curPlayer;
			let inputs = getInput map in
			let indOfGrid = inputs.y / 3 * 3 + inputs.x / 3 in
			let oldGridOwner = Metagrid.Grid.getOwner (List.nth (Metagrid.getGrids map) indOfGrid) in
			let nextCaseOwner = if indOfTurn mod 2 = 0 then Metagrid.Player.O else Metagrid.Player.X in
			let newMap = Metagrid.setCaseOwner inputs.x inputs.y nextCaseOwner map in
			let newGridOwner = Metagrid.Grid.getOwner (List.nth (Metagrid.getGrids map) indOfGrid) in
			if newGridOwner <> oldGridOwner then Printf.printf "%s wins grid %d!" curPlayer indOfGrid;
			gameLoop (indOfTurn + 1) p1 p2 newMap

and startGame () =
	let p1 = getPlayerName () in
	let p2 = getPlayerName ~prev:p1 () in
	let map = Metagrid.newEmptyMetaGrid () in
	gameLoop 0 p1 p2 map;

and getPlayerName ?(prev="") () =
	if prev = "" then
		begin
		print_string "Give me your name player1 (empty = O) : ";
		end
	else 
		print_string "Give me your name player2 (empty = X) : ";
	let name = read_line () in
	if prev <> "" && (name = prev || name = "O") then
		begin
		print_endline "You can't have the same name of the player1 and the name O";
		getPlayerName ~prev:prev ()
		end
	else if prev = "" && name = "X" then
		begin
		print_endline "You can't have the name X";
		getPlayerName ~prev:prev ()
		end
	else
		if name = "" && prev = "" then "O"
		else if name = "" && prev <> "" then "X"
		else name

and getPlay () =
	print_string "Do you want to play ? [yes/no] : ";
	let s = read_line () in
	if s = "yes" then startGame ()
	else if s = "no" then exit 0
	else 
		begin
		print_endline "Incorrect response [yes/no].";
		getPlay ()
		end

let () =
	getPlay ()
