let rec printAndDeleteArmy name arm =
	try
		Printf.printf "Size of army (%s) : %d\n" name (List.length arm#get_army);
		print_endline (List.hd arm#get_army)#to_string;
		printAndDeleteArmy name arm#remove
	with _-> print_char '\n'

let () =
	let dd = new Doctor.doctor "Who" 42 "Robin" in
	let d1 = new Dalek.dalek in
	let d2 = new Dalek.dalek in
	let d3 = new Dalek.dalek in
	let p1 = new People.people "p1" in
	let p2 = new People.people "p2" in
	let p3 = new People.people "p3" in
	let p4 = new People.people "p4" in
	let armyD = new Army.army in
	let armyDa = new Army.army in
	let armyP = new Army.army in
	printAndDeleteArmy "doctors" (armyD#add dd);
	printAndDeleteArmy "daleks" (((armyDa#add d1)#add d2)#add d3);
	printAndDeleteArmy "peoples" ((((armyP#add p1)#add p2)#add p3)#add p4)
