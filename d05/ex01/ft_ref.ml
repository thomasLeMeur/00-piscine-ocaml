type 'a ft_ref = { mutable contents : 'a }

let return c	= { contents = c }
let get r		= r.contents
let set r c		= r.contents <- c
let bind r f	= f r.contents
