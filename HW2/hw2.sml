(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s, ss) =
	case ss of 
		[] => NONE
	  | s' :: ss' => if same_string(s, s') 
					 then SOME ss'
					 else case all_except_option(s, ss') of
							  NONE => NONE
							| SOME list => SOME (s' :: list)

fun get_substitutions1 (sll, s) =
	case sll of 
		[] => []
	  | s' :: sll' => case all_except_option(s, s') of
						  NONE => get_substitutions1(sll', s)
						| SOME list => list @ get_substitutions1(sll', s) 

fun get_substitutions2 (sll, s) = 
	let
		fun aux (sll, s, acc) = 
			case sll of 
				[] => acc
	 | sl' :: sll' => case all_except_option(s, sl') of
										NONE => aux(sll', s, acc)
								 | SOME list => aux(sll', s, acc @ list)
	in 
		aux(sll, s, [])
	end

fun similar_names (sll, full_name) =
	let
		(* from a list of sub first names to a list of full names *)
		fun make_names (f, l, m, ss) =
			case ss of 
				[] => []
			  | s' :: ss' => {first=s', last=l, middle=m} :: make_names(f, l, m, ss')
	in
		case full_name of 
			{first=f, last=l, middle=m} => {first = f, last = l, middle = m} :: make_names(f, l, m, get_substitutions2(sll, f))	
	end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here*)
fun card_color c = 
	case c of 
		(Hearts, _ : rank) => Red
	 |	(Diamonds, _ : rank) => Red
	 |	(Clubs, _ : rank) => Black
	 |	(Spades, _ : rank) => Black

fun card_value c = 
	case c of 
		(_ : suit, Num n) => n
	  | (_ : suit, (Jack | Queen | King)) => 10
	  | (_ : suit, Ace) => 11

fun remove_card (cs, c, e) =
	case cs of 
		[] => raise e
	  | c' :: cs' => if c = c'
					 then cs'
					 else remove_card (cs', c, e)

fun all_same_color cs = 
	case cs of 
		[] => true
	  | c' :: [] => true
	  | c1 :: c2 :: rest => if card_color c1 = card_color c2
							then all_same_color(c2 :: rest)
							else false

fun sum_cards cs = 
	let fun helper (cs, acc) = 
			case cs of 
				[] => acc
			  | c' :: cs' => helper(cs', acc + card_value c')
	in 
		helper (cs, 0)
	end

fun score (cs, goal) = 
	let 
		val sum = sum_cards cs
		val pre = if sum > goal
				  then 3 * (sum - goal)
				  else goal - sum
	in 
		if all_same_color cs
		then pre div 2
		else pre
	end

fun officiate (cs, ms, goal) = 
	let fun round (held_cards, cs, ms, pre) = 
			if pre > goal
			then score(held_cards, goal)
			else 
				case ms of 
					[] => score(held_cards, goal)
				  | Draw :: ms' => (case cs of 
									   [] => score(held_cards, goal)
									 | c' :: cs' => round (c' :: held_cards, cs', ms', pre + card_value c'))
				  | Discard card :: ms' => round(remove_card(held_cards, card, IllegalMove), cs, ms', pre - card_value card)
	in
		round([], cs, ms, 0)
	end
