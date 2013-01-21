fun min (a, b) = if a > b 
		 then b 
		 else a

fun fib x = case x of 
		0 => 1
	      | 1 => 1
	      | x => fib(x-1) + fib(x-2)
       
fun isPrime 1 = false
  | isPrime 2 = true
  | isPrime x  = let 
      fun checkEven num = if num mod 2 = 0 
			  then true 
			  else false
      fun checkMod num = if x mod num <> 0 
			 then if num*num > x 
			      then true 
			      else checkMod(num+1) 
			 else false 
  in 
      if checkEven(x) 
      then false 
      else checkMod(2)
  end
		    
fun sumList [] = 0
  | sumList (a::l) = a + sumList(l)

fun squareList [] = []
  | squareList (a::l) = a*a::squareList(l)

fun max gt = let fun lp curr [] = curr
		   | lp curr (a::l) = if gt(a,curr) then lp a l else lp curr l
	     in lp end
