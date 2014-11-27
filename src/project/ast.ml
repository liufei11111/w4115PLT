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
type mat_op = MTime | MDivide |MAdd | MSub

type expr =
	 Binary_op of expr * bin_op * expr
	| MatBinary_op of expr * mat_op * expr
	| Id of string
	| Float_lit of float
	| Int_lit of int
	| String_lit of string
	| Call of string * expr list (*ComputeSomething(v1,v2,v3);*)
	| VarAssign of expr * expr			(* IDENTIFIER ASSIGN expr SEMICOLON *)
	| Matrix_element of string * expr * expr	(* IDENTIFIER LSQUARE expr RSQUARE LSQUARE expr RSQUARE*)
	(*| MatrixCreate of expr * expr	(* dataType ID LBRACE expr COMMA expr RBRACE SEMICOLON*)	*)
	| Precedence_expr of expr
	| Struct_element of string * string
	 (*type b_expr = 
	| Bool_expr1 of expr * bool_op1 * expr
	| Bool_expr2 of b_expr * bool_op2 * b_expr
	| Precedence_bool_expr of b_expr*)
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
	(*| MatrixCreate (a,b)-> " "^ "[ "^(string_of_expr a)^" , "^(string_of_expr b)^" ]\n"*)
	(*|  | expr EQ     expr { Binary_op($1, Eq, $3) }
  | expr NEQ    expr { Binary_op($1, Neq,   $3) }
  | expr LT     expr { Binary_op($1, Lt,  $3) }
  | expr LEQ    expr { Binary_op($1, Leq,   $3) }
  | expr GT     expr { Binary_op($1, Gt,  $3) }
  | expr GEQ    expr { Binary_op($1, Geq,   $3) }
	| expr AND    expr { Binary_op($1, And,   $3) }
	| expr OR     expr { Binary_op($1, Or,   $3) } *)
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
	| Matrix_element (v,e1,e2)->v ^"["^ string_of_expr e1^" , "^ string_of_expr e2^"] "
  | Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> "void"
	| Precedence_expr(e) -> "( " ^ string_of_expr e ^ " )"
	| Struct_element(struct_id, element_id) -> struct_id ^ "->" ^ element_id
	| _ -> "space"

let string_of_struct_arg arg = arg.id ^ " = " ^ string_of_expr arg.value

(*let rec string_of_b_expr = function 
	 Bool_expr1(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
			Eq -> "=="| Neq -> "!=" | Lt ->"<"| Gt ->">"| Leq -> "<="| Geq -> ">="
      ) ^ " " ^
      string_of_expr e2
	| Bool_expr2 (e1, o, e2) ->
      string_of_b_expr e1 ^ " " ^
      (match o with
			And -> "&&" | Or -> "||" 
      ) ^ " " ^
      string_of_b_expr e2
	| Precedence_bool_expr(e) -> "( " ^ string_of_b_expr e ^ " )"
*)
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


