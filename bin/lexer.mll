{
open Lexing
open Parser
exception SyntaxError of string
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = '-'? digit+  (* regex for integers *)
let id = (alpha) (alpha|digit|'_')* (* regex for identifier *)
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read_token =
  parse
  | "(" { LPAREN }
  ... (* keywords and other characters' regexes *)
  | "printf" {PRINTF }
  | whitespace { read_token lexbuf }
  | "//" { single_line_comment lexbuf (* use our comment rule for rest of line *) }
  | "/*" { multi_line_comment lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf))}
  | id { ID (Lexing.lexeme lexbuf) }
    | '"'      { read_string (Buffer.create 17) lexbuf }
  | newline { next_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ {raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }

and read_single_line_comment = parse
  | newline { next_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ { read_single_line_comment lexbuf }

and read_multi_line_comment = parse
  | "*/" { read_token lexbuf }
  | newline { next_line lexbuf; read_multi_line_comment lexbuf }
  | eof { raise (SyntaxError ("Lexer - Unexpected EOF - please terminate your comment.")) }
  | _ { read_multi_line_comment lexbuf }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  ... (* Other regexes to handle escaping special characters *)
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }


rule main = parse
  | [' ' '\t']+
      { main lexbuf }
  | ['0'-'9']+ as integer
      { INT (int_of_string integer) }
  | "True"
      { BOOL true }
  | "False"
      { BOOL false }
  | '+'
      { PLUS }
  | '-'
      { MINUS }
  | '*'
      { TIMES }
  | '/'
      { DIVIDE }
  | "def"
      { DEF }
  | "int"
      { INTTYPE }
  | ['A'-'Z' 'a'-'z' '_']['0'-'9' 'A'-'Z' 'a'-'z' '_']* as s
      { ID (s) }
  | '('
      { LPAREN }
  | ')'
      { RPAREN }
  | '>'
      { LARGER }
  | '<'
      { SMALLER }
  | ">="
      { EQLARGER }
  | "<="
      { EQSMALLER }
  | "="
      { EQUAL }
  | "!="
      { NOTEQUAL }
  | '~'
      { NOT }
  | "&&"
      { AND }
  | "||"
      { OR }
  | '('
      { LPAREN }
  | ')'
      { RPAREN }
  | "writeint"
      { WRITEINT }
  | '\n'
      { EOL }
  | eof
      { EOF }
  | _
      { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }