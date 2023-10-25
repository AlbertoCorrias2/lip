open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl


let search token l = List.filter(fun (x,_) -> x = token) l = []


let rec count token l = match l with
	[] -> 0
	| hd::tl -> if(hd = token) then 1 + count token tl else count token tl
	
let sort l = List.sort(fun (_,x1) (_,x2) -> compare x2 x1) l

let rec order n l = match l with
	[] -> []
	| hd::tl -> if(n > 0) then hd::order (n-1) tl else []

let create_list token = 
	let rec create_element tok l = match tok with
		[] -> l
		| hd::tl -> if(search hd l) then let update_list = (hd, 1 + count hd tl)::l in
		create_element tl update_list else create_element tl l in
	create_element token []
	

(* frequency : int -> 'a list -> ('a * int) list *)
let frequency n token = order n (sort (create_list token))
