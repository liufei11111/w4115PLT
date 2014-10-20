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

type bin_op = Add | Sub | Times | Divide
type mat_op = MTime | MDivide |MAdd | MSub
type bool_op1 = Eq | Neq | Lt | Gt | Leq | Geq
type bool_op2 =  And | Or

type expr =
	 Binary_op of expr * bin_op * expr
	| MatBinary_op of expr * mat_op * expr
	| Id of string
	| Float_lit of float
	| Int_lit of int
	| String_lit of string
	| Boolean of string
	| Call of string * expr list (*ComputeSomething(v1,v2,v3);*)
	| VarAssign of expr * expr			(* IDENTIFIER ASSIGN expr SEMICOLON *)
	| ElemAssign of string * expr * expr * expr	(* IDENTIFIER LSQUARE expr RSQUARE LSQUARE expr RSQUARE ASSIGN expr SEMICOLON *)
	(*| MatrixCreate of expr * expr	(* dataType ID LBRACE expr COMMA expr RBRACE SEMICOLON*)	*)
	|	Noexpr
type b_expr = 
	  Bool_expr1 of expr * bool_op1 * expr
	| Bool_expr2 of b_expr * bool_op2 * b_expr

type stmt =
	  Block of stmt list					(* LBRACE Statement_list RBRACE *)
	| Expr of expr							(* expr SEMICOLON *)
	| If of b_expr * stmt * stmt						(* IF LPAREN expr RPAREN Block *)
	| For of expr * b_expr * expr * stmt		(* FOR LPAREN expr SEMICOLON expr SEMICOLON expr Statement SEMICOLON *)
	| While of b_expr * stmt					(* WHILE LPAREN expr RPAREN Statement SEMICOLON *)
	| Return of expr


type func_dec = {
	ret : dataType;
	func_name : string;
	formals : var_dec list;
	locals : var_dec list;
	body : stmt list;
}

type program = var_dec list * func_dec list

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
let rec string_of_expr = function
    Float_lit(l) -> string_of_float l
	| Int_lit(l) -> string_of_int l
	| String_lit(l) -> l
  | Id(s) -> s
	(*| MatrixCreate (a,b)-> " "^ "[ "^(string_of_expr a)^" , "^(string_of_expr b)^" ]\n"*)
  | Binary_op(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Times -> "*" | Divide -> "/"
      ) ^ " " ^
      string_of_expr e2
	| MatBinary_op(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	MAdd -> "+." | MSub -> "-." | MTime -> "*." | MDivide -> "/."
			) ^ " " ^
      string_of_expr e2
  | VarAssign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
	| ElemAssign(v,e1,e2,e3)->v ^"["^ string_of_expr e1^" , "^ string_of_expr e2^"] = "^ string_of_expr e2^"\n"
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> "void"
	| Boolean(s) -> s
	| _ -> "space"


let rec string_of_b_expr = function 
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
			

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_b_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_b_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_b_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_b_expr e ^ ") " ^ string_of_stmt s


let string_of_vdecl vdecl =    vdecl.vname ^ " " ^(string_of_dataType ( vdecl.vtype)) ^ ";\n"

let string_of_fdecl fdecl =
 string_of_dataType fdecl.ret ^" " ^ fdecl.func_name ^ "(" 
^ String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "vdecls: \n " (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "fdecls: \n" (List.map string_of_fdecl funcs) ^ "\n" 


