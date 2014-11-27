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

