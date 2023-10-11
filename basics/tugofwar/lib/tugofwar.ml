(* tokens *)
type token = A | B | X

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
             
let toklist_of_string s = explode s;;
	

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let rec valid l = match l with
	[] -> false
	| 'A'::'='::tl -> if(List.filter(fun x -> x = 'A') tl = []) then valid tl else false
	| '='::'B'::tl -> List.filter(fun x -> x = '=' || x = 'A') tl = []
	| '='::tl -> List.filter(fun x -> x = 'A') tl = []
	| 'B'::tl -> List.filter(fun x -> x = '=' || x = 'A') tl = []
	| _::tl -> valid tl;;

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let rec count l tok = match l with
	[] -> 0
	| hd::tl when hd = tok -> 1 + count tl tok
	| _::tl -> 0 + count tl tok;;


let win l = if(count l 'A' > count l 'B') then A else if(count l 'A' = count l 'B') then X else B;;

(* val string_of_winner : token -> string *)
let string_of_winner w = if (w = X) then "It's a draw" else if w = A then "The winner of the tug of war is A" else "The winner of the tug of war is B";;
