class dalek =
object
	val _name : string = Random.self_init ();
						"Dalek" ^ (String.make 1 ((char_of_int ((Random.int 95) + 32))))
								^ (String.make 1 ((char_of_int ((Random.int 95) + 32))))
								^ (String.make 1 ((char_of_int ((Random.int 95) + 32))))
	val _hp : int = 100
	val mutable _shield : bool = true

	method to_string = _name ^ " [hp: " ^ (string_of_int _hp) ^ ", with " ^ (if _shield == true then "" else "no ") ^ "shield]"
	method talk = let ind = Random.int 4 in 
					if ind == 0 then print_endline "Explain! Explain!"
					else if ind == 0 then print_endline "Exterminate! Exterminate!"
					else if ind == 0 then print_endline "I obey"
					else print_endline "You are the Doctor! You are the enemy of the Daleks!"
	method exterminate (other : People.people) = other#die; _shield <- (not _shield)
	method die = print_endline "Emergency Temporal Shift!"
end
