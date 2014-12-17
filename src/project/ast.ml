type dataType =
	| Int 
	| Float
	| String
	| Matrix
	| Option
	| Structure
	| Boolean
	| Void

type var_dec = {
	vname : string;
	vtype : dataType;
}

type mat_dec = {
	mname : string;
	mtype : dataType;
	mrow : int;
	mcol : int;
}

type bin_op = Add | Sub | Times | Divide| And | Or | Eq | Neq | Lt | Gt | Leq | Geq
type mat_op = MTime | MDivide |MAdd | MSub | MITime | MIDivide |MIAdd | MISub
type mat_uop = MTranspose | MInversion | MDeterminant

type expr =
	 Binary_op of expr * bin_op * expr
	| MatBinary_op of expr * mat_op * expr
	| Id of string
	| Float_lit of float
	| Int_lit of int
	| String_lit of string
	| Call of string * expr list (*ComputeSomething(v1,v2,v3);*)
	| VarAssign of string * expr			(* IDENTIFIER ASSIGN expr SEMICOLON *)
	| Matrix_element_assign of string * expr * expr * expr
	| Struct_element_assign of string * string * expr
	| Matrix_element of string * expr * expr	(* IDENTIFIER LSQUARE expr RSQUARE LSQUARE expr RSQUARE*)
	| Precedence_expr of expr
	| Struct_element of string * string
	| Bool_lit of int
	| MatUnary_op of expr * mat_uop
	|	Noexpr

type struct_arg = {
	id : string;
	value : expr;
}	

type stmt =
	  Block of stmt list					(* LBRACE Statement_list RBRACE *)
	| Expr of expr							(* expr SEMICOLON *)
	| If of expr * stmt * stmt						(* IF LPAREN expr RPAREN Block *)
	| For of expr * expr * expr * stmt		(* FOR LPAREN expr SEMICOLON expr SEMICOLON expr Statement SEMICOLON *)
	| While of expr * stmt					(* WHILE LPAREN expr RPAREN Statement SEMICOLON *)
	| Return of expr
	| Vardec of var_dec
	| Matdec of mat_dec
	| Structdec of string * struct_arg list
	| Optiondec of string * struct_arg list


type func_dec = {
	ret : dataType;
	func_name : string;
	formals : var_dec list;
	body : stmt list;
}

type program = stmt list * func_dec list

(* "Pretty printed" version of the AST, meant to generate a MicroC program
   from the AST.  These functions are only for pretty-printing (the -a flag)
   the AST and can be removed. *)
let string_of_dataType = function
	  Int -> "int"
	| Float -> "float"
	| String -> "string"
	| Matrix -> "matrix"
	| Option -> "option"
	| Structure -> "structure"
	| Boolean -> "bool"
	| Void -> "void"

			
let string_of_vdecl vdecl =  string_of_dataType vdecl.vtype ^ " " ^ vdecl.vname
let string_of_mdecl mdecl = string_of_dataType mdecl.mtype ^ " " ^ mdecl.mname ^
														"(" ^ string_of_int mdecl.mrow ^ ", " ^ string_of_int mdecl.mcol ^ ")"

let rec string_of_expr = function
    Float_lit(l) -> string_of_float l
	| Int_lit(l) -> string_of_int l
	| String_lit(l) -> "\"" ^ l ^ "\""
  | Id(s) -> s
  | Binary_op(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
			Add -> "+" | Sub -> "-" | Times -> "*" | Divide -> "/" | Eq -> "=="
			| Neq -> "!=" 			| Lt -> "<" | Leq -> "<=" | Gt -> ">" | Geq -> ">=" 
			| And -> "&&" | Or -> "||" 
      ) ^ " " ^
      string_of_expr e2
	| MatBinary_op(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
			MAdd -> "+." | MSub -> "-." | MTime -> "*." | MDivide -> "/."
			| MIAdd -> "+.." | MISub -> "-.." | MITime -> "*.." | MIDivide -> "/.."
			) ^ " " ^
      string_of_expr e2
  | VarAssign(v, e) -> v ^ " = " ^ string_of_expr e
 	| Matrix_element_assign(id, r, c, e) -> id^ "["^ string_of_expr r^" , "^ string_of_expr c^ "] = "^ string_of_expr e
	| Struct_element_assign(id, i, e) -> id^ " -> "^ i^ " = "^ string_of_expr e
	| Matrix_element (v,e1,e2)->v ^"["^ string_of_expr e1^" , "^ string_of_expr e2^"] "
	| Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> "void"
	| Precedence_expr(e) -> "( " ^ string_of_expr e ^ " )"
	| Struct_element(struct_id, element_id) -> struct_id ^ "->" ^ element_id
	| Bool_lit(b) -> (match b with | 1 -> " 1 " | 0 -> " 0 ")
	| MatUnary_op(e, o) ->  
		(match o with
		MTranspose -> "Transpose" | MInversion -> "Inversion" | MDeterminant -> "Determinant"	
		) ^ "(" ^ string_of_expr e ^ ")"
	| _ -> "space"

let string_of_struct_arg arg = arg.id ^ " = " ^ string_of_expr arg.value

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
	| Structdec (id, argList) -> "Structure " ^ id ^ " = {" ^ String.concat ", " (List.map string_of_struct_arg (List.rev argList)) ^ "};\n"
	| Optiondec (id, argList) -> "Option " ^ id ^ " = {" ^ String.concat ", " (List.map string_of_struct_arg (List.rev argList)) ^ "};\n"

	 
let string_of_fdecl fdecl =
 string_of_dataType fdecl.ret ^" " ^ fdecl.func_name ^ "(" 
^ String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
	String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (stmts, funcs) =
  String.concat "\n" (List.map string_of_stmt (List.rev stmts)) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl (List.rev funcs)) ^ "\n" 


