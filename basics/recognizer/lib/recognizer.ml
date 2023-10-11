let binaryDigits s = List.filter(fun x -> x='0' || x='1') s = s;;

let lang1 s = binaryDigits s && s<>[];;

let lang2 s = binaryDigits s && match s with
	[] -> false
	| _::tl -> List.filter(fun x -> x='1') tl = tl;;

let lang3 s = binaryDigits s && match s with
	[] -> false
|	'0'::tl -> (match List.rev tl with
		'0'::_ -> true
	|	_ -> false)
|	_ -> false;;

let lang4 s = binaryDigits s && List.length(List.filter(fun x -> x='1') s) = 2;; 

let rec couple s = match s with
	[] -> true
|	el::el2::tl -> if(el <> el2) then false else couple tl
|	_ -> false;;

let lang5 s = binaryDigits s && List.length s >= 2 && couple s;;
	
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
