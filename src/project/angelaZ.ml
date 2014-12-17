
(* angelaZ compiler
	1. give stdin to scanner, get tokens
	2. give tokens to parser, get AST
	3. give AST to analyzer, get semantic tree
	4. give semantic tree to java converter, get java tree
	5. give java tree to java code generator, get java code
	6. give java code to java compiler, get executable
	7. run java executable
*)

type action =  Sast | Java

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [  ("-s", Sast); ("-j", Java)]
  else Java in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
  Sast ->
  		let result = Typecheck.check_program program in
  		()
  | Java ->
      let _ = Javagen.gen_program "Output" program in
      print_string "Success! Compiled to java/output.java\n"
