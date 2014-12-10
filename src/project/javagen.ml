open Printf
open Random
open Ast
(**
	Previous results
**)
let string_of_dataType = function
	  Int -> "int"
	| Float -> "float"
	| String -> "String"
	| Matrix -> "Matrix"
	| Option -> "Option"
	| Structure -> "Structure"
	| Boolean -> "boolean"
	| Void -> "void"

			
let string_of_vdecl vdecl =  string_of_dataType vdecl.vtype ^ " " ^ vdecl.vname
let string_of_mdecl mdecl = string_of_dataType mdecl.mtype ^ " " ^ mdecl.mname ^
														"=new Matrix(" ^ string_of_int mdecl.mrow ^ ", " ^ string_of_int mdecl.mcol ^ ")"
(*put struct id into the declaration list inside struct declaration*)
let rec tuple_id (id,nums) = match nums with
| [] -> []
| head :: tail -> (id,head):: tuple_id (id,tail)
(*string for expression*)
let rec string_of_expr = function
    Float_lit(l) -> string_of_float l
	| Int_lit(l) -> string_of_int l
	| String_lit(l) -> "\"" ^ l ^ "\""
  | Id(s) -> s
  | Binary_op(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
			| (*And | Or | Eq | Neq | Lt | Gt | Leq | Geq*)
			Add -> "+" | Sub -> "-" | Times -> "*" | Divide -> "/" | Eq -> "=="
			| Neq -> "!=" 			| Lt -> "<" | Leq -> "<=" | Gt -> ">" | Geq -> ">=" 
			| And -> "&&" | Or -> "||" 
      ) ^ " " ^
      string_of_expr e2
	| MatBinary_op(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
			MAdd -> "+." | MSub -> "-." | MTime -> "*." | MDivide -> "/."
			) ^ " " ^
      string_of_expr e2
  | VarAssign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
	| Matrix_element (v,e1,e2)->v ^"["^ string_of_expr e1^"]["^ string_of_expr e2^"] "
  | Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> "void"
	| Precedence_expr(e) -> "( " ^ string_of_expr e ^ " )"
	| Struct_element(struct_id, element_id) -> struct_id ^ ".get(" ^ element_id^")"
	| _ -> "space"
(*string for struct*)
let string_of_struct_arg (struct_id,arg) = struct_id^".valMap.put(\""^arg.id ^ "\" , " ^ string_of_expr arg.value^")"

(*string of statement*)	 
let rec string_of_stmt = function
    Block(stmts) -> "{\n  " ^ String.concat "  " (List.map string_of_stmt stmts) ^ "}\n"
	| Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  (*| If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s*)
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ") " ^
      string_of_stmt s1 ^ "else " ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
	| Vardec(vdecl) -> string_of_vdecl vdecl ^ ";\n"
	| Matdec(mdecl) -> string_of_mdecl mdecl ^ ";\n"
	| Structdec (struct_id, argList) -> "Structure " 
	^ struct_id ^ " = new Structure();\n" ^
	 String.concat ";\n " 
	(List.map (string_of_struct_arg ) (tuple_id (struct_id,(List.rev argList)) )) ^ ";\n"
	| Optiondec (struct_id, argList) -> "Option  " 
	^ struct_id ^ " = new Option();\n" ^
	 String.concat ";\n " 
	(List.map (string_of_struct_arg ) (tuple_id (struct_id,(List.rev argList)) )) ^ ";\n"
	| Struct_element_assign (struct_id,member_id, expr) ->  struct_id^".valMap.put(\"" ^member_id^"\","^string_of_expr expr^");\n" 
(*function decoration*)	 
let string_of_fdecl fdecl =
 "public static "^string_of_dataType fdecl.ret ^" " ^ fdecl.func_name ^ "(" 
^ String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
	String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"
(*convert ast into string*)
let string_of_program_gen (stmts, funcs)=
  String.concat "\n" (List.map string_of_stmt (List.rev stmts)) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl (List.rev funcs)) ^ "\n" 
(*io function*)

let rec writeToFile fileName progString = 
  let file = open_out ("java/" ^ fileName ^ ".java") in
    let content=fprintf file "%s"  progString in
		"Write finished!"
(*generate program and output to file*)
let gen_program fileName prog = (*have a writetofile*)
  let programString = string_of_program_gen (fst prog ,snd prog ) in
  let out = sprintf 
	"  \npublic class %s\n{\n%s\n}" fileName programString in
  	writeToFile fileName out
