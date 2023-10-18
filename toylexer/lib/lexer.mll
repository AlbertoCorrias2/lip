{
  open Token
}

let white = [' ' '\t']+
let capital = ['A'-'Z']
let lowVowel = ['a' 'e' 'i' 'o' 'u']
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let vowels = ['a' 'e' 'i' 'o' 'u' 'A' 'E' 'I' 'O' 'U']
let digit = ['0'-'9']
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let consonants = ['b'-'d' 'f'-'h' 'j'-'n' 'p'-'t' 'v'-'z' 'B'-'D' 'F'-'H' 'J'-'N' 'P'-'T' 'V'-'Z']
let id = letter chr*
let atok = capital chr*
let btok = lowVowel+
let ctok = consonants* vowels? consonants*
let dtok = ('-')? num* ('.')? digit+
let etok = ("0x"|"0X") chr+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | atok { ATOK }
  | btok { BTOK }
  | ctok { CTOK }
  | dtok { DTOK }
  | etok { ETOK }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF } 
