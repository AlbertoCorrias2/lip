type token =
  | LPAREN
  | RPAREN
  | ASSIGN
  | PLUS
  | SEQ
  | ID of string
  | CONST of string
  | ATOK
  | BTOK
  | CTOK
  | DTOK
  | ETOK
  | EOF

let string_of_token = function
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | ASSIGN -> "ASSIGN"
  | PLUS -> "PLUS"
  | SEQ -> "SEQ"
  | ID(s) -> "ID(" ^ s ^ ")"
  | CONST(s) -> "CONST(" ^ s ^ ")"
  | ATOK -> "ATOK"
  | BTOK -> "BTOK"
  | CTOK -> "CTOK"
  | DTOK -> "DTOK"
  | ETOK -> "ETOK"
  | EOF -> "EOF"
