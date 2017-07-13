module Player =
struct
	type t = X | O | Nil | None
end

module Grid =
struct
	module Case =
	struct
		type t = Player.t

		let newEmptyCase ()	= Player.Nil
		let newCase value	= value

		let getOwner self = self
		let getRepr = function
			| Player.X	-> "X"
			| Player.O	-> "O"
			| _	-> "-"
	end

	type t = { cases : Case.t list; owner : Player.t }

	let newEmptyGrid () =	{
							cases =	[
									Case.newEmptyCase (); Case.newEmptyCase (); Case.newEmptyCase ();
									Case.newEmptyCase (); Case.newEmptyCase (); Case.newEmptyCase ();
									Case.newEmptyCase (); Case.newEmptyCase (); Case.newEmptyCase ()
									];
							owner = Player.Nil
							}
	let newGrid cases owner = { cases = cases; owner = owner }

	let getCases self = self.cases

	let isEmptyCase x y self = 
		if self.owner != Player.Nil then false
		else if x >= 3 || x < 0 || y < 0 || y >= 3 then false
		else (Case.getOwner (List.nth self.cases (y * 3 + x))) == Player.Nil

	let setCaseOwner x y owner self = 
		if (isEmptyCase x y self) == false then
			self
		else
			let rec loop ind owner curGrid endGrid = match curGrid with
				| []	->	endGrid
				| hd::tl->	if ind <> 0 then loop (ind - 1) owner tl (endGrid @ [hd])
							else loop (ind - 1) owner tl (endGrid @ [(Case.newCase owner)])
			in
			{ cases = (loop (y * 3 + x) owner self.cases []); owner = self.owner }

	let isLost self = 
		if self.owner <> Player.Nil then
			if self.owner = Player.None then true
			else false
		else
			let rec loop = function
				| []	->	true
				| hd::tl->	if (Case.getOwner hd) = Player.Nil then false
							else loop tl
			in
			loop self.cases

	let getOwner self =
		if self.owner <> Player.Nil then
			self.owner
		else if (isLost self) = true then
			Player.None
		else
			let rec loop last cases = match cases with
				| []	->	last
				| hd::tl->	let curOwner = Case.getOwner hd in
							if last <> curOwner then Player.Nil
							else loop curOwner tl
			in
			let own = loop (Case.getOwner (List.nth self.cases 0)) ([(List.nth self.cases 0); (List.nth self.cases 1); (List.nth self.cases 2)]) in
			if own <> Player.Nil then own
			else
				let own = loop (Case.getOwner (List.nth self.cases 0)) ([(List.nth self.cases 0); (List.nth self.cases 3); (List.nth self.cases 6)]) in
				if own <> Player.Nil then own
				else
					let own = loop (Case.getOwner (List.nth self.cases 0)) ([(List.nth self.cases 0); (List.nth self.cases 4); (List.nth self.cases 8)]) in
					if own <> Player.Nil then own
					else
						let own = loop (Case.getOwner (List.nth self.cases 1)) ([(List.nth self.cases 1); (List.nth self.cases 4); (List.nth self.cases 7)]) in
						if own <> Player.Nil then own
						else
							let own = loop (Case.getOwner (List.nth self.cases 2)) ([(List.nth self.cases 2); (List.nth self.cases 5); (List.nth self.cases 8)]) in
							if own <> Player.Nil then own
							else
								let own = loop (Case.getOwner (List.nth self.cases 2)) ([(List.nth self.cases 2); (List.nth self.cases 4); (List.nth self.cases 6)]) in
								if own <> Player.Nil then own
								else
									let own = loop (Case.getOwner (List.nth self.cases 3)) ([(List.nth self.cases 3); (List.nth self.cases 4); (List.nth self.cases 5)]) in
									if own <> Player.Nil then own
									else loop (Case.getOwner (List.nth self.cases 6)) ([(List.nth self.cases 6); (List.nth self.cases 7); (List.nth self.cases 8)])

	let getRepr self =
		if self.owner == Player.X then
			["\\   / "; "  X   "; "/   \\ "]
		else if self.owner == Player.O then
			["/ - \\ "; "|   | "; "\\ - / "]
		else
			let rec loop curRepr finalRepr grid = match grid with
				| []	->	finalRepr
				| hd::tl->	let repr = (Case.getRepr hd) in
							if (List.length tl) mod 3 == 0 then
								loop "" (finalRepr @ [curRepr ^ repr ^ " "]) tl
							else
								loop (curRepr ^ repr ^ " ") finalRepr tl
			in
			loop "" [] self.cases
end

type t = { grids : Grid.t list; owner : Player.t }

let newEmptyMetaGrid () =	{
							grids =	[
									Grid.newEmptyGrid (); Grid.newEmptyGrid (); Grid.newEmptyGrid ();
									Grid.newEmptyGrid (); Grid.newEmptyGrid (); Grid.newEmptyGrid ();
									Grid.newEmptyGrid (); Grid.newEmptyGrid (); Grid.newEmptyGrid ()
									];
							owner = Player.Nil
							}
let newMetaGrid grids owner = { grids = grids; owner = owner }

let getGrids self = self.grids

let isEmptyCase x y self = 
	if x >= 9 || x < 0 || y < 0 || y >= 9 then false
	else
		let xInGrid = x / 3 in
		let yInGrid = y / 3 in
		let xInCase = x mod 3 in
		let yInCase = y mod 3 in
		Grid.isEmptyCase xInCase yInCase (List.nth self.grids (yInGrid * 3 + xInGrid))

let setCaseOwner x y owner self = 
	if (isEmptyCase x y self) == false then
		self
	else
		let xInGrid = x / 3 in
		let yInGrid = y / 3 in
		let xInCase = x mod 3 in
		let yInCase = y mod 3 in
		let indOfGrid = yInGrid * 3 + xInGrid in
		let rec loop ind owner curMetaGrid endMetaGrid = match curMetaGrid with
			| []	->	endMetaGrid
			| hd::tl->	if ind <> 0 then loop (ind - 1) owner tl (endMetaGrid @ [hd])
						else loop (ind - 1) owner tl (endMetaGrid @ [(Grid.setCaseOwner xInCase yInCase owner hd)])
		in
		let newGrids = loop indOfGrid owner self.grids [] in
		let newOwner = Grid.getOwner (List.nth newGrids indOfGrid) in
		if newOwner == Player.Nil then { grids = newGrids; owner = self.owner }
		else
			let rec changeGrids final ind grids = match grids with
				| []	->	final
				| hd::tl->	if ind <> 0 then changeGrids (final @ [hd]) (ind - 1) tl
							else changeGrids (final @ [(Grid.newGrid (Grid.getCases hd) newOwner)]) (ind - 1) tl
			in
			{ grids = (changeGrids [] indOfGrid newGrids) ; owner = self.owner }

let getOwner self =
	let rec loop last grids = match grids with
		| []	->	last
		| hd::tl->	let curOwner = Grid.getOwner hd in
					if last <> curOwner then Player.Nil
					else loop curOwner tl
	in
	let own = loop (Grid.getOwner (List.nth self.grids 0)) ([(List.nth self.grids 0); (List.nth self.grids 1); (List.nth self.grids 2)]) in
	if own <> Player.Nil then own
	else
		let own = loop (Grid.getOwner (List.nth self.grids 0)) ([(List.nth self.grids 0); (List.nth self.grids 3); (List.nth self.grids 6)]) in
		if own <> Player.Nil then own
		else
			let own = loop (Grid.getOwner (List.nth self.grids 0)) ([(List.nth self.grids 0); (List.nth self.grids 4); (List.nth self.grids 8)]) in
			if own <> Player.Nil then own
			else
				let own = loop (Grid.getOwner (List.nth self.grids 1)) ([(List.nth self.grids 1); (List.nth self.grids 4); (List.nth self.grids 7)]) in
				if own <> Player.Nil then own
				else
					let own = loop (Grid.getOwner (List.nth self.grids 2)) ([(List.nth self.grids 2); (List.nth self.grids 5); (List.nth self.grids 8)]) in
					if own <> Player.Nil then own
					else
						let own = loop (Grid.getOwner (List.nth self.grids 2)) ([(List.nth self.grids 2); (List.nth self.grids 4); (List.nth self.grids 6)]) in
						if own <> Player.Nil then own
						else
							let own = loop (Grid.getOwner (List.nth self.grids 3)) ([(List.nth self.grids 3); (List.nth self.grids 4); (List.nth self.grids 5)]) in
							if own <> Player.Nil then own
							else loop (Grid.getOwner (List.nth self.grids 6)) ([(List.nth self.grids 6); (List.nth self.grids 7); (List.nth self.grids 8)])

let getRepr self =
	let printGridLine ind reprs =
		(List.nth (List.nth reprs 0) 0) ^ "| " ^ (List.nth (List.nth reprs 1) 0) ^ "| " ^ (List.nth (List.nth reprs 2) 0) ^ "\n" ^
		(List.nth (List.nth reprs 0) 1) ^ "| " ^ (List.nth (List.nth reprs 1) 1) ^ "| " ^ (List.nth (List.nth reprs 2) 1) ^ "\n" ^
		(List.nth (List.nth reprs 0) 2) ^ "| " ^ (List.nth (List.nth reprs 1) 2) ^ "| " ^ (List.nth (List.nth reprs 2) 2) ^ "\n" ^
		(if ind < 2 then "---------------------\n" else "")
	in
	let rec loop repr ind grid = match grid with
		| a::b::c::tl	-> loop (repr ^ (printGridLine ind [(Grid.getRepr a); (Grid.getRepr b); (Grid.getRepr c)])) (ind + 1) tl
		| _				-> repr
	in
	loop "" 0 self.grids

let isLost self =
	let rec loop = function
		| []	->	true
		| hd::tl->	if (Grid.getOwner hd) = Player.Nil then false
					else loop tl
	in
	loop self.grids
