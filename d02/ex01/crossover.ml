let crossover a b =
	if a == [] || b == [] then
		[]
	else
		let rec isInLst ref lst = match lst with
			| []	-> false
			| f::s	-> if f == ref then true else (isInLst ref s)
		and crossA final lst = match lst with
			| []	-> final
			| f::s	-> if (isInLst f b) == true && (isInLst f final) == false then (crossA (final@[f]) s) else (crossA final s)
		in
		crossA [] a
