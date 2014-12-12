open Printf
open Ast
(**
	Previous results
**)
let string_of_dataType = function
	  Int -> "int"
	| Float -> "double"
	| String -> "String"
	| Matrix -> "Matrix"
	| Option -> "Option"
	| Structure -> "Structure"
	| Boolean -> "boolean"
	| Void -> "void"

			(*type dataType =
	| Int 
	| Float
	| String
	| Matrix
	| Option
	| Structure
	| Boolean
	| Void*)
let string_of_vdecl vdecl =  string_of_dataType vdecl.vtype ^ " " ^ vdecl.vname ^ match vdecl.vtype with
| Int -> "=0"
	| Float-> "=0.0"
	| String -> "=null"
	| Matrix-> ""
	| Option -> ""
	| Structure -> ""
	| Boolean -> "=false"
	| Void  -> ""

let string_of_global_vdecl vdecl =  "static "^string_of_dataType vdecl.vtype ^ " " ^ vdecl.vname ^ match vdecl.vtype with
| Int -> "=0"
	| Float-> "=0.0"
	| String -> "=null"
	| Matrix-> ""
	| Option -> ""
	| Structure -> ""
	| Boolean -> "=false"
	| Void  -> ""


let string_of_vdecl_argument vdecl =  string_of_dataType vdecl.vtype ^ " " ^ vdecl.vname 
let string_of_mdecl mdecl = string_of_dataType mdecl.mtype ^ " " ^ mdecl.mname ^
														"= new Matrix(" ^ string_of_int mdecl.mrow ^ ", " ^ string_of_int mdecl.mcol ^ ")"
(*put struct id into the declaration list inside struct declaration*)
let rec tuple_id (id,nums) = match nums with
| [] -> []
| head :: tail -> (id,head):: tuple_id (id,tail)
(*string for expression*)
let rec string_of_expr_javagen = function
    Float_lit(l) -> string_of_float l
	| Int_lit(l) -> string_of_int l
	| String_lit(l) -> "\"" ^ l ^ "\""
  | Id(s) -> s
  | Binary_op(e1, o, e2) ->
      string_of_expr_javagen e1 ^ " " ^
      (match o with
			| (*And | Or | Eq | Neq | Lt | Gt | Leq | Geq*)
			Add -> "+" | Sub -> "-" | Times -> "*" | Divide -> "/" | Eq -> "=="
			| Neq -> "!=" 			| Lt -> "<" | Leq -> "<=" | Gt -> ">" | Geq -> ">=" 
			| And -> "&&" | Or -> "||" 
      ) ^ " " ^
      string_of_expr_javagen e2
			(*matrix binary operations*)
	| MatBinary_op(e1, o, e2) ->
      (match o with
			MAdd -> "MatrixMathematics.add("^string_of_expr_javagen e1^" ,"^string_of_expr_javagen e2^")" 
			| MSub -> "MatrixMathematics.subtract("^string_of_expr_javagen e1^" ,"^string_of_expr_javagen e2^")" 
			| MTime -> "MatrixMathematics.multiply("^string_of_expr_javagen e1^" ,"^string_of_expr_javagen e2^")"  
			| MDivide -> "MatrixMathematics.multiply("^string_of_expr_javagen e1^" ,"^"MatrixMathematics.inverse("^string_of_expr_javagen e2^"))"
			| MITime  ->  "("^string_of_expr_javagen e1^").multiplyByConstant("^string_of_expr_javagen e2^")"
			| MIDivide  ->  "("^string_of_expr_javagen e1^").multiplyByConstant(1.000/("^string_of_expr_javagen e2^"))"
			| MIAdd  ->  "("^string_of_expr_javagen e1^").addByConstant("^string_of_expr_javagen e2^")"
			| MISub  ->  "("^string_of_expr_javagen e1^").addByConstant(-1*("^string_of_expr_javagen e2^"))"
			) 
   (* assign codes *)   
  | VarAssign(v, e) -> v ^ " = " ^ string_of_expr_javagen e
	| Matrix_element_assign (id,indexR,indexC,assignV)-> id^".data" ^"["^ string_of_expr_javagen indexR^"]["^ string_of_expr_javagen indexC^"] = "^string_of_expr_javagen assignV
	| Struct_element_assign (struct_id,member_id, expr) ->  struct_id^".valMap.put(\"" ^member_id^"\","^string_of_expr_javagen expr^");\n" 
	| Matrix_element (v,e1,e2)->v^".data" ^"["^ string_of_expr_javagen e1^"]["^ string_of_expr_javagen e2^"] "
	(* functions that are built in are listed here*)
  | Call(f, el) -> (match f with
		| "printM" -> "("^String.concat ", " (List.map string_of_expr_javagen el)^").print()"
		| "toInt" -> "Integer.parseInt("^String.concat ", " (List.map string_of_expr_javagen  el)^")"
		| "toString" -> "ToString.toString(" ^ String.concat ", " (List.map string_of_expr_javagen  el)^")"
		| "print" -> "System.out.println(ToString.toString(" ^ String.concat ", " (List.map string_of_expr_javagen  el)^"))"
		|_  ->  f^"(" ^ String.concat ", " (List.map string_of_expr_javagen el) ^ ")"
		)
  | Noexpr -> "void"
	| Precedence_expr(e) -> "( " ^ string_of_expr_javagen e ^ " )"
	| Struct_element(struct_id, element_id) -> struct_id^".valMap" ^ ".get(\"" ^ element_id^"\")"
	| _ -> "space"
(*string for struct*)
let string_of_struct_arg (struct_id,arg) = struct_id^".valMap.put(\""^arg.id ^ "\" , " ^ string_of_expr_javagen arg.value^")"
(*string of global statement*)	 
let rec string_of_global_stmt = function
	Vardec(vdecl) -> string_of_global_vdecl vdecl ^ ";\n"
	| _  -> ""
(*string of statement*)	 
let rec string_of_stmt = function
    Block(stmts) -> "{\n  " ^ String.concat "  " (List.map string_of_stmt stmts) ^ "}\n"
	| Expr(expr) -> string_of_expr_javagen expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr_javagen expr ^ ";\n"
  (*| If(e, s, Block([])) -> "if (" ^ string_of_expr_javagen e ^ ")\n" ^ string_of_stmt s*)
  | If(e, s1, s2) ->  "if (" ^ string_of_expr_javagen e ^ ") " ^
      string_of_stmt s1 ^ "else " ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr_javagen e1  ^ " ; " ^ string_of_expr_javagen e2 ^ " ; " ^
      string_of_expr_javagen e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr_javagen e ^ ") " ^ string_of_stmt s
	| Vardec(vdecl) -> string_of_vdecl vdecl ^ ";\n"
	| Matdec(mdecl) -> string_of_mdecl mdecl ^ ";\n"
	| Structdec(struct_id, argList) -> "Structure " 
	^ struct_id ^ " = new Structure();\n" ^
	 String.concat ";\n " 
	(List.map (string_of_struct_arg ) (tuple_id (struct_id,(List.rev argList)) )) ^ ";\n"
	| Optiondec (struct_id, argList) -> "Option  " 
	^ struct_id ^ " = new Option();\n" ^
	 String.concat ";\n " 
	(List.map (string_of_struct_arg ) (tuple_id (struct_id,(List.rev argList)) )) ^ ";\n"
(*function decoration*)	 
let string_of_fdecl fdecl =
 "public static "^string_of_dataType fdecl.ret ^" " ^ fdecl.func_name ^ "(" 
^ String.concat ", " (List.map string_of_vdecl_argument fdecl.formals) ^ ")\n{\ntry{" ^
	String.concat "" (List.map string_of_stmt fdecl.body) ^
	(*exception simply exits*)
  "}catch(Exception e){"^(match fdecl.ret with
| Int -> "return -1"
	| Float-> "return 0.0"
	| String -> "return null"
	| Matrix-> "return null"
	| Option -> "return null"
	| Structure -> "return null"
	| Boolean -> "return false"
	| Void  -> "return "
	)
	^";}}\n"
(*convert ast into string*)
let string_of_program_gen (stmts, funcs)=
  String.concat "\n" (List.map string_of_global_stmt (List.rev stmts)) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl (List.rev funcs)) ^ "\n" 
(*io function*)

let  writeToFile fileName progString = 
  let file = open_out ("java/" ^ fileName ^ ".java") in
    let content=fprintf file "%s"  progString in
		"Write finished!"
(*generate program and output to file*)
let gen_program fileName prog = (*have a writetofile*)
  let programString = string_of_program_gen (fst prog ,snd prog ) in
  let out = sprintf 
	"\npublic class %s\n{\n%s%s\n}" fileName programString "public static void main(String[] args){ try{main(0,\"\");}catch(Exception e){return ;}}" in
  	writeToFile fileName out
