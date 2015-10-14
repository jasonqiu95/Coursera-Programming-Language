(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

fun count_wildcards p = g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p = g (fn () => count_wildcards p) (fn x => String.size x) p

fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p
	
(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

fun only_capitals xs = 
	let 
		fun capitals x = 
			Char.isUpper (String.sub(x, 0))
	in
		List.filter capitals xs
	end

fun longest_string1 xs = 
	let 
		fun bigger_string (s1, s2) = 
			if String.size s1 > String.size s2
			then s1
			else s2
	in
		foldl bigger_string "" xs
	end

fun longest_string2 xs = 
	let 
		fun bigger_string (s1, s2) = 
			if String.size s1 >= String.size s2
			then s1
			else s2
	in
		foldl bigger_string "" xs
	end

val longest_capitalized = fn sl => (longest_string1 o only_capitals) sl

fun rev_string s = 
	let fun concat cl = 
			case cl of 
				[] => ""
			  | x' :: xs' => (Char.toString x') ^ concat xs'
	in concat (rev (String.explode s))
	end

fun first_answer f ans_list = 
	case ans_list of 
		[] => raise NoAnswer
	  | x' :: xs' => case f x' of 
						 NONE => first_answer f xs'
					   | SOME v => v

fun all_answers f xs = 
	let 
		fun helper f acc xs = 
			case xs of 
				[] => SOME acc
			  | x' :: xs' => case (f x') of 
								NONE => NONE
							  | SOME x => helper f (acc @ x) xs'
	in helper f [] xs
	end


