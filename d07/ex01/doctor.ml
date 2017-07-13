class doctor name age sidekickName =
object (self)
	val _name : string = name
	val _age : int = age
	val _sidekick : People.people = new People.people sidekickName
	val _hp : int = 100

	method to_string = _name ^ " [hp: " ^ (string_of_int _hp) ^ ", age: " ^ (string_of_int _age) ^ ", sidekick: " ^ _sidekick#to_string ^ "]"
	method talk = print_endline "Hi! I'm the Doctor!"
	method travel_in start arrival = print_endline "--\n||\n--\n"; {< _name = _name; _age = (_age + (arrival - start)); _sidekick = _sidekick; _hp = _hp >}

	method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

	method private regenerate = {< _name = _name; _age = _age; _sidekick = _sidekick; _hp = 100 >}

	initializer print_endline (name ^ ", a new Doctor, is bornt !")
end
