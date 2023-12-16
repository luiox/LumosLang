(* parser.ml *)

(* 定义语法分析器模块 Parser *)
module Parser = struct
  (* 在这里定义语法分析器的具体实现 *)
  (* 可以包括语法规则、语法分析函数等 *)

rule token = parse
  | [' ' '\t' '\r' '\n']  { token lexbuf } (* 忽略空白字符 *)
  | ['a'-'z' 'A'-'Z']+ as id { ID(id) } (* 匹配标识符 *)
  | ['0'-'9']+ as num { NUM(int_of_string num) } (* 匹配数字 *)
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIVIDE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | eof { EOF }
  | _ as char { raise (LexicalError ("Unexpected character: " ^ Char.escaped char)) };;

type stream = { mutable chr: char option; mutable line_num: int; chan: in_channel }

let open_stream file = { chr=None; line_num=1; chan=open_in file }
let close_stream stm = close_in stm.chan
let read_char stm = match stm.chr with
                        None -> let c = input_char stm.chan in
                                if c = '\n' then
                                    let _ = stm.line_num <- stm.line_num + 1 in c
                                else c
                      | Some c -> stm.chr <- None; c
let unread_char stm c = stm.chr <- Some c

let is_digit c = let code = Char.code c in 
                  code >= Char.code('0') && code <= Char.code('9')

let is_alpha c = let code = Char.code c in
                  (code >= Char.code('A') && code <= Char.code('Z')) ||
                  (code >= Char.code('a') && code <= Char.code('z'))
 
(* token *)
type token = Begin | End
           | Identifier of string 
           | Read | Write 
           | Literal of int 
           | Assign 
           | LeftParen | RightParen  
           | AddOp | SubOp
           | Comma | Semicolon

type scanner = { mutable last_token: token option; stm: stream }

exception Syntax_error of string

let syntax_error s msg = raise (Syntax_error (msg ^ " on line " ^ (string_of_int s.stm.line_num)))



end