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

let f = 10;;

end