fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(s: string, strings: string list) =
let 
  fun all_except_option_acc(acc: string list, rest: string list, exists: bool) =
    case (rest, exists) of 
         (xs, true) => SOME (acc @ xs)
       | ([], false) => NONE
       | (x::xs, false) => if same_string(s, x)
                     then all_except_option_acc(acc, xs, true)
                     else all_except_option_acc(x::acc, xs, false)
in
  all_except_option_acc([], strings, false)
end

fun get_substitutions1(subs: string list list, s: string) = 
  case subs of
       [] => []
     | x::xs => case all_except_option(s, x) of
                 NONE => get_substitutions1(xs, s)
               | SOME res => res @ get_substitutions1(xs, s) 

fun get_substitutions2(subs: string list list, s: string) = 
let
  fun get_subs(acc: string list, rest: string list list) =
    case rest of
         [] => acc
       | x::xs => case all_except_option(s, x) of
                   NONE => get_subs(acc, xs)
                 | (SOME a) => get_subs(acc @ a, xs)
in
  get_subs([], subs)
end

fun similar_names(
  subs: string list list, { first: string, middle: string, last: string }) =
let
  fun iter([], acc) = acc
    | iter(x::xs, acc) = iter(xs, {first = x, middle = middle, last = last}::acc)
in
  iter(first::get_substitutions2(subs, first), [])
end

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(Spades, _) = Black
  | card_color(Clubs, _) = Black
  | card_color(Diamonds, _) = Red
  | card_color(Hearts, _) = Red
  
fun card_value(_, Ace) = 11
  | card_value(_, Num n) = n
  | card_value(_, _) = 10

fun remove_card([], _, e: exn) = raise e
  | remove_card(x::xs, card, e: exn) = 
    if x = card
    then xs 
    else x::remove_card(xs, card, e)

fun all_same_color([]) = true
  | all_same_color(x1::[]) = true
  | all_same_color(x1::x2::xs) = card_color(x1) = card_color(x2) andalso
  all_same_color(x2::xs) 

fun sum_cards(cards: card list) = 
let
  fun iter([], acc) = acc
    | iter(x::xs, acc) = iter(xs, acc + card_value(x))
in
  iter(cards, 0)
end

fun score(cards: card list, goal: int) =
let
  val sum = sum_cards(cards)
  val preliminary = if sum > goal
                    then 3 * (sum - goal)
                    else (sum - goal) 
in
  if all_same_color(cards)
  then preliminary div 2
  else preliminary
end

fun officiate(cards: card list, moves: move list, goal: int) =
let
  fun iter(_, [], goal, hand) = score(hand, goal) 
    | iter([], _, goal, hand) = score(hand, goal)
    | iter(deck, Discard disc::moves, goal, hand) = iter(deck, moves, goal,
        remove_card(hand, disc, IllegalMove))
    | iter(c::deck, Draw::moves, goal, hand) = 
        if sum_cards(hand) + card_value(c) > goal
        then score(c::hand, goal)
        else iter(deck, moves, goal, c::hand) 
in
  iter(cards, moves, goal, [])
end
