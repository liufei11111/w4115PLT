open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in

  let prog = Parser.program Scanner.token lexbuf in
  let result = string_of_program (fst prog ,snd prog ) in
  print_endline result
