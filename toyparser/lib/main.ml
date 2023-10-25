open Ast

(* parse : string -> ast *)

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

type result = int option

let string_of_result n = string_of_int n
    
(* eval : ast -> result *)
    
let rec eval = function
    Const(n) -> Some(n)
  | Add(e1,e2) -> Some(eval e1 + eval e2)
  | Sub(e1,e2) -> Some(eval e1 - eval e2)
  | Mul(e1,e2) -> Some(eval e1 * eval e2)
  | Div(e1,e2) -> if(eval e2 = Some(0)) then None else eval e1 / eval e2

                    
