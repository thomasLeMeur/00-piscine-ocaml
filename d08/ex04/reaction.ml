class virtual reaction (startM : (Molecule.molecule * int) list) (endM : (Molecule.molecule * int) list) =
object
	method virtual get_start : (Molecule.molecule * int) list
	method virtual get_result : (Molecule.molecule * int) list
	method virtual balance : reaction
	method virtual is_balanced : bool
end
