class ['a] army =
object
	val _army : 'a list = []

	method get_army = _army

	method add unity = {< _army = _army @ [unity] >}
	method remove = {< _army = List.tl _army >}
end
