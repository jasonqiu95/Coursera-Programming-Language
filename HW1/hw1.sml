(* Chen Qiu, Coursera Programming Language, Dan Grossman, HW1, 10/09/14*)

val months = ["January", "February", "March", "April",
"May", "June", "July", "August", "September", "October", "November", "December"]

val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun is_older (d1 : int*int*int, d2 : int*int*int) =
	if #1 d1 <> #1 d2
	then #1 d1 < #1 d2
	else if #2 d1 <> #2 d2
	then #2 d1 < #2 d2
	else if #3 d1 <> #3 d2
	then #3 d1 < #3 d2
	else false

fun number_in_month (ds : (int*int*int) list, m : int) = 
	if null ds 
	then 0
	else if #2 (hd ds) = m
	then 1 + number_in_month (tl ds, m)
	else number_in_month (tl ds, m)

fun number_in_months (ds : (int*int*int) list, ms : int list) = 
	if null ms
	then 0
	else number_in_month (ds, hd ms) + number_in_months(ds, tl ms)
	
fun dates_in_month (ds : (int*int*int) list, m : int) =
	if null ds
	then []
	else if #2 (hd ds) = m
	then hd ds :: dates_in_month(tl ds, m)
	else dates_in_month(tl ds, m)

fun dates_in_months (ds : (int*int*int) list, ms : int list) =
	if null ms
	then []
	else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms)

fun get_nth (ss : string list, n : int) =
	if n = 1
	then hd ss
	else get_nth(tl ss, n - 1)
	
fun date_to_string (d : int*int*int) =
	get_nth(months, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)

fun number_before_reaching_sum (sum : int, li : int list) =
	if hd li >= sum
	then 0
	else 1 + number_before_reaching_sum(sum - hd li, tl li)

fun what_month (day : int) =
	number_before_reaching_sum(day, days_in_month) + 1
	
fun month_range (day1 : int, day2 : int) =
	if day1 > day2 
	then []
	else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (ds : (int*int*int) list) = 
	if null ds
	then NONE
	else 
		let val tl_ans = oldest(tl ds)
		in if isSome(tl_ans) andalso is_older(valOf(tl_ans), hd ds)
		then tl_ans
		else SOME (hd ds)
		end
