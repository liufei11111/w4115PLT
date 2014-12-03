open Ast
(*************************
**** SAST ****************
*************************)

(*****Types Annotated******)
type expr_t =
   Binary_op_t of expr_t * bin_op * expr_t * dataType
  | MatBinary_op_t of expr_t * mat_op * expr_t * dataType
  | Id_t of string * dataType
  | Float_lit_t of float * dataType
  | Int_lit_t of int * dataType
  | String_lit_t of string * dataType
  | Call_t of string * expr_t list * dataType
  | VarAssign_t of expr_t * expr_t * dataType
  | Matrix_element_t of string * expr_t * expr_t * dataType
  | Precedence_expr_t of expr_t * dataType
	| Struct_element_t of string * string * dataType
  | Noexpr_t of dataType

type stmt_t =
    Block_t of stmt_t list
  | Expr_t of expr_t 
  | If_t of expr_t * stmt_t * stmt_t 
  | For_t of expr_t * expr_t * expr_t * stmt_t
  | While_t of expr_t * stmt_t 
  | Return_t of expr_t
  | Vardec_t of var_dec * dataType
  | Matdec_t of mat_dec * dataType
	| Structdec_t of string * expr_t list * dataType
	| Optiondec_t of string * expr_t list * dataType

type func_dec_t = {
	ret : dataType;
	func_name : string;
	formals : var_dec list;
	body : stmt_t list;
}

type program_t = stmt_t list * func_dec_t list

(*******************Functions for debugging typechecking*****************)

let rec string_of_expr_t = function
    Float_lit_t(l,t) -> string_of_float l ^ string_of_dataType t
	| Int_lit_t(l,t) -> string_of_int l ^ string_of_dataType t
	| String_lit_t(l,t) -> "\"" ^ l ^ "\"" ^ string_of_dataType t
  | Id_t(s,t) -> s ^ string_of_dataType t
  | Binary_op_t(e1, o, e2,t) ->
      string_of_expr_t e1 ^ " " ^
      (match o with
			| Add -> "+" | Sub -> "-" | Times -> "*" | Divide -> "/" | Eq -> "=="
			| Neq -> "!=" 			| Lt -> "<" | Leq -> "<=" | Gt -> ">" | Geq -> ">=" 
			| And -> "&&" | Or -> "||" 
      ) ^ " " ^
      string_of_expr_t e2 ^ string_of_dataType t
	| MatBinary_op_t(e1, o, e2,t) ->
      string_of_expr_t e1 ^ " " ^
      (match o with
			MAdd -> "+." | MSub -> "-." | MTime -> "*." | MDivide -> "/."
			) ^ " " ^
      string_of_expr_t e2 ^ string_of_dataType t
  | VarAssign_t(v, e,t) -> string_of_expr_t v ^ " = " ^ string_of_expr_t e ^ string_of_dataType t
	| Matrix_element_t (v,e1,e2,t)->v ^"["^ string_of_expr_t e1^" , "^ string_of_expr_t e2^"] " ^ string_of_dataType t
  | Call_t(f, el,t) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr_t el) ^ ")"^ string_of_dataType t
  | Noexpr_t (t)-> "void" ^ string_of_dataType t
	| Precedence_expr_t(e,t) -> "( " ^ string_of_expr_t e ^ " )"^ string_of_dataType t
	| Struct_element_t(struct_id, element_id,t) -> struct_id ^ "->" ^ element_id^ string_of_dataType t
	| _ -> "space"

let rec string_of_stmt_t = function
    Block_t(stmts) -> "{\n  " ^ String.concat "  " (List.map string_of_stmt_t stmts) ^ "}\n"
	| Expr_t(expr) -> string_of_expr_t expr ^ ";\n"
  | Return_t(expr) -> "return " ^ string_of_expr_t expr ^ ";\n"
  (*| If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s*)
  | If_t(e, s1, s2) ->  "if (" ^ string_of_expr_t e ^ ") " ^
      string_of_stmt_t s1 ^ "else " ^ string_of_stmt_t s2
  | For_t(e1, e2, e3, s) ->
      "for (" ^ string_of_expr_t e1  ^ " ; " ^ string_of_expr_t e2 ^ " ; " ^
      string_of_expr_t e3  ^ ") " ^ string_of_stmt_t s
  | While_t(e, s) -> "while (" ^ string_of_expr_t e ^ ") " ^ string_of_stmt_t s
	| Vardec_t(vdecl,t) -> string_of_vdecl vdecl ^ string_of_dataType t ^ ";\n" 
	| Matdec_t(mdecl,t) -> string_of_mdecl mdecl ^ string_of_dataType t^ ";\n"
	| Structdec_t (id, argList,t) -> "Structure " ^ id ^ " = {" ^ String.concat ", " (List.map string_of_expr_t (List.rev argList)) ^ string_of_dataType t^ "};\n"
	| Optiondec_t (id, argList,t) -> "Option " ^ id ^ " = {" ^ String.concat ", " (List.map string_of_expr_t (List.rev argList)) ^ string_of_dataType t^ "};\n"
 
	 
let string_of_fdecl_t fdecl_t =
 string_of_dataType fdecl_t.ret ^" " ^ fdecl_t.func_name ^ "(" 
^ String.concat ", " (List.map string_of_vdecl fdecl_t.formals) ^ ")\n{\n" ^
	String.concat "" (List.map string_of_stmt_t fdecl_t.body) ^
  "}\n"

let string_of_program_t (stmts, funcs) =
  String.concat "\n" (List.map string_of_stmt_t (List.rev stmts)) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl_t (List.rev funcs)) ^ "\n" 


