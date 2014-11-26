open Ast
open Sast
open List
(**************************************** Symbol Tables *)
type symbol_table = {
  parent : symbol_table option;
  mutable variables : (string * Ast.dataType) list;
}

type environment = {
  mutable func_return_type : Ast.dataType option; (* Function return type *)
  scope : symbol_table;        (* symbol table for varibles *)
  mutable functions : (string * Ast.dataType list * Ast.dataType) list; (* symbol table for global functions, nested function declaration not supported*)
}
(****************************************** Initial*)
let new_env : environment = 
  let core =[("print", [String], Void)] in 
  let utils = [("size", [String], Int)] in
  let s = { variables = []; parent = None }
  in
  {scope = s ; func_return_type = None; functions = concat [core; utils];}

let inner_scope (env : environment) : environment =
  let s = { variables = []; parent = Some(env.scope) } in
  { env with scope = s; }

(****************************************** Utils*)
let rec find_vars (env_scope : symbol_table) (var_name : string) : Ast.dataType option= 
  try
    let (_, typ) = List.find (fun (s, _) -> s = var_name) env_scope.variables in
    Some(typ)
  with Not_found ->
    match env_scope.parent with
    | Some(p) -> find_vars p var_name
    | _ -> None 

let find_funcs (env : environment) (var_name : string) = 
	try
    let (_, typ) = List.find (fun (s, _, _) -> s = var_name) env.functions in
    Some(typ)
  with Not_found -> None

let get_type (e : Sast.expr_t): Ast.dataType = 
  match e with 
   Binary_op_t(e1,o,e2,t) -> t
  | MatBinary_op_t(e1,o,e2,t) -> t
  | Id_t(s,t) -> t
  | Float_lit_t(s,t) -> t
  | Int_lit_t(s,t) -> t
  | String_lit_t(s,t) -> t
  | Boolean(s,t) -> t
  | Call_t(s, vl, t) ->t
  | VarAssign_t(e1,e2,t)->t
  | Matrix_element(s,e1,e2,t)->t 
  | Precedence_expr_t(e1,t)->t 
	| Struct_element_t(s1,s2,t)->t 
  | Noexpr_t(t)->t

(****************************************** Annotate and elementary check*)
let annotate_assign (env : environment) (e1 : Ast.expr) (e2 : Ast.expr) : Sast.expr_t = (* For variable assign*)
  let e2_a = annotate_expr env e2 in 
  match e1 with
  | Id(x) -> 
    let src_type = get_type e2_a in
    let typ = find_vars env.scope x in
    (match typ with
      | Float -> 
        if (src_type = Float) || (src_type = Int)
        then VarAssign_t(Id_t(x, typ), e2_a, typ)
        else raise(Failure("variable "^x^" need to be float type"))
      | None -> 
          raise(Failure("variable "^x^" not declared before assignment"))
      | _ -> 
        if dst_typ <> typ
        then raise(Failure("variable "^x^" need to be assigned with same type"))
        else VarAssign_t(Id_t(x, typ), e2_a, typ)
      )
  | _ -> raise(Failure("Assignment need to be applied to a variable"))

let annotate_expr (env : environment) (e : Ast.expr): Sast.expr_t = 
  match e with 
  Binary_op(e1, op, e2) -> 
		let e1_a = annotate_expr env e1 in
		let e2_a = annotate_expr env e2
		in
			let e1_t = get_type e1_a in
			let e2_t = get_type e2_a in
			(match e1_t with
				| Float -> 
					if (e2_t = Float) || (e2_t = Int)
					then Binary_op_t(e1_a, op, e2_a, Float)
					else raise(Failure("Binary operation has un-consistent types"))
				| Int ->
					(match e2_t with
					| Int -> Binary_op_t(e1_a, op, e2_a, Int)
					| Float -> Binary_op_t(e1_a, op, e2_a, Float)
					| _ -> raise(Failure("Binary operation has un-consistent types")))
				| _ -> 
					if e1_t = e2_t
					then Binary_op_t(e1_a, op, e2_a, e1_t)
					else raise(Failure("Binary operation has un-consistent types")))
			Binary_op_t(e1_a, op, e2_a, datat)
  | MatBinary_op(e1, op, e2) ->
		let e1_a = annotate_expr env e1 in
		let e2_a = annotate_expr env e2 in
			let e1_t = get_type e1_a in
			let e2_t = get_type e2_a in
				if (e1_t <> Matrix) || (e2_t <> Matrix)
				then raise(Failure("Matrix operation has to be Matrix type"))
				else MatBinary_op_t(e1_a, op, e2_a, Matrix)
  | Id(s) ->
		let typ = find_vars env.scope s in
    (match typ with
      | Some(x) -> Id_t(s, x)
      | None -> raise(Failure ("Unrecognized identifier " ^ s ^ "."))
		)
  | Float_lit(f) ->Float_lit_t(f,Float)
  | Int_lit(i) ->Int_lit_t(i,Int)
  | String_lit(s) ->String_lit_t(s,String)
  | Boolean(b) ->Boolean_t(b,Boolean)
  | Call(name, exl)-> 
      let exl_a = List.map (fun x -> annotate_expr env x) exl in
      let ret_type = find_funcs env name in
			(match ret_type with
			Some(tp) -> Call_t(name, exl_a, tp) (*Check the input variables are the same type *)
			|None ->raise(Failure ("Unrecognized identifier " ^ s ^ ".")))
  | VarAssign(e1, e2)->  annotate_assign env e1 e2
  | ElemAssign(id, e1,e2,e3) -> 
		let e1_a = annotate_expr env e1 in 
		let e1_type = get_type e1_a in
		if e1_type = Int
		then let e2_a = annotate_expr env e2 in
			let e2_type = get_type e2_a in
			if e2_type= Int
			then let e3_a = annotate_expr env e3 in
				let e3_type = get_type e3_a in
				if e3_type = Int || e3_type = Float
				then let var_typ = find_vars env.scope id in
					if var_typ = Matrix
					then ElemAssign_t(id, e1_a, e2_a, e3_a, Float)
					else raise(Failure ("Element assign only allowed to Matrix type" ))
				else raise(Failure ("Assigned value to matrix has to be of type Int or Float" ))
			else raise(Failure ("Index of matrix should be of type Int" ))
		else raise(Failure ("Index of matrix should be of type Int" ))
  | Precedence_expr(e) -> annotate_expr env e
  | Noexpr -> Noexpr_t(Void)

let annotate_b_expr (env : environment) (e : Ast.b_expr): Sast.b_expr_t =  (*TODO add single Boolean variable*)
  match e with
  Bool_expr1(e1, bo, e2) ->
    let e1_a = annotate_expr e1 in
    let e2_a = annotate_expr e2
    in 
      let e1_type = get_type e1_a in
      let e2_type = get_type e2_a
      in
        if (e1_type <> Boolean) ||  (e2_type <> Boolean)
        then raise(Failure("Both sides of boolean expression need to be "))
        else Bool_expr1(e1_t, bo, e2_t)
  | Bool_expr2(be1, bo ,be2)-> Bool_expr2_t(be1_t, bo, be2_t)
  | Precedence_bool_expr(be1) -> Precedence_bool_expr_t(be1) 

let annotate_stmt (env : environment) (s : Ast.stmt): Sast.stmt_t = 
  match s with 
    Block(stmtlist) ->
      let env' = inner_scope env
      in 
        let stmt_t_list =  annotate_stmts stmtlist env'
        in
          Block_t(stmt_t_list)
    | Expr(e) -> 
      let expr_a = annotate_expr env e
      in
        Expr_t(expr_a)
    | If(be, body1, body2) ->
      let be_a = annotate_b_expr env be in
      let env' = inner_scope env 
      in
        let body1_a = annotate_stmts env' body1 in
        let body2_a = annotate_stmts env' body2
        in
          If_t(be_a, body1_a, body2_a)
    | For(ae1, be, ae2, body)->
      let ae1_a = 
        (match ae1 with
        | VarAssign(e1, e2) -> annotate_assign env e1 e2
        | None -> None
        | _ -> raise(Failure("Need assignment in for loop header"))) in
      let be_a = 
        (match be with
        | Some(x) -> Some(annotate_b_expr env x)
        | None -> None) in
      let ae2_a = 
        (match ae2 with
        | VarAssign(e1, e2) -> annotate_assign env e1 e2
        | None -> None) in
      let env' = inner_scope env
      in
        let body_a = annotate_stmts env' body 
        in
          For_t(ae1_a, be_a, ae2_a, body_a)
    | While(be, body)  ->
      let be_a = annotate_b_expr env be in
      let env' = inner_scope env 
      in
        let body_a = annotate_stmts env' body 
        in
          While_t(be_a, body_a)       
    | Return(e) ->(*TODO: watch to see if handle void return type*)
      let e_a = annotate_expr env e 
      in 
        let return_type = get_type e_a
        in
          if return_type <> env.func_return_type
          then raise(Failure("actual return type is not same with function return type"))
          else 
            Return_t(e_a)
    | Vardec(vd) ->(*TODO and check whether it is void*)
      let var_type = vd.vtype in
      let var_name = vd.vname 
      in
        if is_keyword var_name
         then raise(Failure("Cannot use keyword " ^ var_name ^ " as variable name"))
        else 
          let exist_v = find_vars env.scope var_name
          in
            if exist_v 
              then raise(Failure("Variable name " ^ var_name ^ " already been used."))
            else 
							let a = env.scope.variables@[(var_name, var_type)] in
              Vardec_t(vd, var_type)
    | Matdec(md) ->(*TODO and check whether it is void*)
      let var_type = vd.vtype in
      let var_name = vd.vname
      in
        if is_keyword var_name
          then raise(Failure("Cannot use keyword " ^ var_name ^ "  as variable name"))
        else 
          if var_type <> Matrix
           then raise(Failure("The declaration of  " ^ var_name ^ " only apply to Matrix"))
          else
            let exist_v = find_vars env.scope var_name
            in
              if exist_v 
                then raise(Failure("Variable name " ^ var_name ^ "  already been used."))
              else 
								let a = env.scope.variables@[(var_name, var_type)] in
                Matdec_t(vd, var_type)


let get_formal_type (vardec : Ast.var_dec) : Ast.dataType  = var_dec.vtype
	
let addFormal (env: environment) (vardec : Ast.var_dec) = 
	let v_name = vardec.vname in 
	if is_keyword v_name
	 then raise(Failure("Cannot use keyword " ^ name ^ " as variable name"))
	else
		let exist_v = find_vars env.scope v_name in
		if exist_v
		  then raise(Failure("Variable name " ^ name ^ " already been used."))
		else 
			env.scope.variables@[(v_name,v_type)];
let annotate_fun (env: environment) (func : Ast.func_dec) : Sast.func_dec_t = 
	let ret_type = func.ret in
	let name = func.func_name in
  	if is_keyword name
          then raise(Failure("Cannot use keyword " ^ name ^ " as function name"))
    else 
        let exist_f = find_funcs env name
            in
              if exist_f 
                then raise(Failure("Function name " ^ name ^ " already been used."))
              else 
  							let formals_list = func.formals in
								  List.iter (fun x -> addFormal env x) formals_list in
								    let body_stmts = func.body in
								      let body_a = annotate_stmts env body_stmts in
								        let data_type_list = List.map (fun x-> get_formal_type x) formals_list in
								          env.functions @ [(name,data_type_list,ret_type)]

let annotate_funcs  (env : environment) (funcs : Ast.func_dec list): Sast.func_dec_t list  = 
	List.map (fun x -> annotate_func env x) funcs

let annotate_stmts  (env : environment) (stmts : Ast.stmt list) : Sast.stmt_t list =
  List.map (fun x -> annotate_stmt env x) stmts

(****************************************** Checking Program and main function exists *)

let check_program prog = 
  let globals = prog.fst in
	let functions = prog.snd in
	let env = new_env() 
	in
    let global_stmts_a = List.map annotate_stmts env globals in
    let funcs_a = List.map annotate_funcs env functions in 
    print_endline "\nSemantic analysis completed successfully.\nGenerating...\n"