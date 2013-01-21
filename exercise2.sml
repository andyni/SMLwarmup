datatype expr = 
	 NUM of int 
	 | PLUS of expr * expr
	 | MINUS of expr * expr
	 | TIMES of expr * expr
	 | DIV of expr * expr
	 | F of expr list * (int list -> int)

fun eval (NUM x) = x
  | eval (PLUS (x,y)) = (eval x) + (eval y) 
  | eval (MINUS (x,y)) = (eval x) - (eval y) 
  | eval (TIMES (x,y)) = (eval x) * (eval y)
  | eval (DIV (x,y)) = (eval x) div (eval y) 
  | eval (F (l, f)) = f (map eval l)

fun flatten l = foldr (op @) [] l

fun map f l  = foldr (fn(x,list) => (f x)::list) [] l

fun filter f l = foldr (fn(x,list) => if (f x) 
				      then x::list 
				      else list) [] l 

fun count f l = foldr (fn(x,count) => if (f x) 
				      then count+1 
				      else count) 0 l

fun mapPartial f l = foldr (fn(x,list) => if isSome (f x) 
					  then (valOf (f x))::list 
					  else list) [] l
