type phosphate = string ;;
type deoxyribose = string ;;
type nucleobase = A | T | C | G | None ;;

type nucleotide =
{
	p : phosphate;
	d : deoxyribose;
	n : nucleobase
} ;;

let generate_nucleotide = function
	| 'A'	-> {p = "phosphate"; d = "deoxyribose"; n = A}
	| 'T'	-> {p = "phosphate"; d = "deoxyribose"; n = T}
	| 'C'	-> {p = "phosphate"; d = "deoxyribose"; n = C}
	| 'G'	-> {p = "phosphate"; d = "deoxyribose"; n = G}
	| _		-> {p = "phosphate"; d = "deoxyribose"; n = None}
;;
