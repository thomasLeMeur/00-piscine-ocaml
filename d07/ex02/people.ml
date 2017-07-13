class people name =
object
	val _name	= name
	val _hp		= 100

	method to_string	= _name ^ " [hp: " ^ (string_of_int _hp) ^ "]"
	method talk			= print_endline ("I'm " ^ _name ^ "! Do you know the Doctor?")
	method die			= print_endline "Aaaarghh!"

	initializer print_endline (name ^ " is bornt !")
end
