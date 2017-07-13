class beast (health : int) (energy : int) (hygiene : int) (happy : int) =
object (self)
	val _health = max 0 (min 100 health)
	val _energy = max 0 (min 100 energy)
	val _hygiene = max 0 (min 100 hygiene)
	val _happy = max 0 (min 100 happy)

	method get_health = _health
	method get_energy = _energy
	method get_hygiene = _hygiene
	method get_happy = _happy
end

class interface =
object (self)
	val _side = 1280
	val _title = "Rush 01"
	val _saveFileName = "save.itama"
	val mutable _monster = new beast 100 100 100 100
	val _bars = Array.make 4 (GRange.progress_bar ())

	method get_monster = _monster
	method set_monster m = _monster <- m

	method private saveFile =
		try
			let out_file = open_out _saveFileName in
			Printf.fprintf out_file "%d;%d;%d;%d\n" _monster#get_health _monster#get_energy _monster#get_hygiene _monster#get_happy;
			close_out out_file;
			print_endline "Game saved, see you :)"
		with _ -> print_endline "An error occured during the save, sorry :p"

	method private loadFile =
		if (Sys.file_exists _saveFileName) then
			begin
			try
				let in_file = open_in _saveFileName in
				let line = input_line in_file in
				close_in in_file;
				Sys.remove _saveFileName;
				let vals = List.map int_of_string (Str.split (Str.regexp ";") line) in
				if List.length vals != 4 then
					invalid_arg "Wrong file format";
				print_endline "Game loaded, welcome back :)";
				_monster <- new beast 0 0 0 0;
				self#updateBars ~to_quit:true (List.nth vals 0) (List.nth vals 1) (List.nth vals 2) (List.nth vals 3)
			with _ -> print_endline "An error occured during the load, sorry :p"
			end

	method private eat () = self#updateBars (25) (-10) (-20) (5)
	method private thunder () = self#updateBars (-20) (25) (0) (-20)
	method private bath () = self#updateBars (-20) (-10) (25) (5)
	method private kill () = self#updateBars (-20) (-10) (0) (20)
	method private timer () = self#updateBars (-1) (0) (0) (0); true
	method private destroy () = self#saveFile; GMain.Main.quit ()

	method private updateBars ?(to_quit=false) he en hy ha =
		self#set_monster (new beast	(_monster#get_health + he)
									(_monster#get_energy + en)
									(_monster#get_hygiene + hy)
									(_monster#get_happy + ha));
		_bars.(0)#set_fraction ((float_of_int _monster#get_health) /. 100.0);
		_bars.(1)#set_fraction ((float_of_int _monster#get_energy) /. 100.0);
		_bars.(2)#set_fraction ((float_of_int _monster#get_hygiene) /. 100.0);
		_bars.(3)#set_fraction ((float_of_int _monster#get_happy) /. 100.0);
		if (_monster#get_health == 0) || (_monster#get_energy == 0) || (_monster#get_hygiene == 0) || (_monster#get_happy == 0) then
			begin
			print_endline "GAME OVER";
			GMain.Main.quit ();
			if to_quit then
				exit 0
			end

	method private createMenu vbox =
		let createMenuItem hbox title frac =
			let vbox = GPack.vbox ~packing:hbox#add () in
			ignore (GMisc.label ~text:title ~packing:vbox#add ());
			let bar = GRange.progress_bar ~packing:vbox#add () in
			bar#set_fraction ((float_of_int frac) /. 100.0);
			bar
		in
		let hbox = GPack.hbox ~spacing:100 ~border_width:100 ~height:0 ~packing:vbox#add () in
		_bars.(0) <- createMenuItem hbox "HEALTH" _monster#get_health;
		_bars.(1) <- createMenuItem hbox "ENERGY" _monster#get_energy;
		_bars.(2) <- createMenuItem hbox "HYGIENE" _monster#get_hygiene;
		_bars.(3) <- createMenuItem hbox "HAPPY" _monster#get_happy

	method private createImage vbox =
		ignore (GMisc.image ~pixbuf:(GdkPixbuf.from_file "pikachu.jpg") ~packing:vbox#add ())

	method private createButtons vbox window =
		let createButton hbox name callback =
			let button = GButton.button ~label:name ~packing:hbox#add () in
			ignore (button#connect#clicked ~callback:callback)
		in
		let hbox = GPack.hbox ~spacing:100 ~border_width:100 ~height:0 ~packing:vbox#add () in
		createButton hbox "EAT" self#eat;
		createButton hbox "THUNDER" self#thunder;
		createButton hbox "BATH" self#bath;
		createButton hbox "KILL" self#kill

	method run =
		ignore (GtkMain.Main.init ());
		ignore (GMain.Timeout.add 1000 self#timer);
		let window = GWindow.window	~resizable:false ~width:_side ~height:_side ~title:_title () in
		ignore (window#connect#destroy ~callback:self#destroy);
		let vbox = GPack.vbox ~packing:window#add () in
		self#createMenu vbox;
		self#createImage vbox;
		self#createButtons vbox window;
		self#loadFile;
		window#show ();
		GMain.Main.main ()
end

let () =
	(new interface)#run
