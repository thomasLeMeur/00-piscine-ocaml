module type APP =
sig
	type project = string * string * int
	val zero : project
	val combine : project -> project -> project
	val fail : project -> project
	val success : project -> project
end

module App : APP =
struct
	type project = string * string * int
	let zero = ("", "", 0)
	let combine ((s1, status1, grade1) : project) ((s2, status2, grade2) : project) = let mean = (grade1 + grade2) / 2 in ((s1 ^ s2), (if mean >= 80 then "succeed" else "failed"), mean)
	let fail ((s, status, grade) : project) = (s, "failed", 0)
	let success ((s, status, grade) : project) = (s, "succeed", 80)
end

let () =
	let print_proj ((s, status, grade) : App.project) =
		Printf.printf "The proj is described as %s, he's marked as %s with a grade of %d\n" s status grade
	in
	print_proj App.zero;
	print_proj (App.fail App.zero);
	print_proj (App.success App.zero);
	print_proj (App.combine ("Cou", "", 10) ("Cui", "", 20));
	print_proj (App.combine ("Co", "", 80) ("Cu", "", 85))
