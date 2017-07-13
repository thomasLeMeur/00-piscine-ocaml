let f = function
	| 0	-> "Qu'est ce qui est jaune et qui attend ... Jonathan"
	| 1	-> "Ta mère est tellement grande qu'elle habite au premier étage"
	| 2	-> "Mon premier est lui"
	| 3	-> "C'est un mec qui rentre dans un café et fait plouf"
	| _	-> "C'est un mec qui rentre dans un bar et qui dit : 'Salut c'est moi' ... mais en fait c'était pas lui"


let () =
	Random.self_init ();
	let tab = Array.init 5 f in 
	print_endline tab.(Random.int 5)
