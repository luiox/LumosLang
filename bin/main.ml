let () = print_endline "Hello, World!"

type Token

type identifier = 
  | Variable of Var_name.t
  | ObjField of Var_name.t * Field_name.t

