open Ast
open Sast
(****************************************** Debug Functions*)
let print_var elem = print_endline (fst elem ^ "\t" ^ string_of_dataType (snd elem)^" is in scope")
	
let rec print_vars_list = function (*string*dataType list to print*)
[] -> print_string "empty vars\n"
| e::l -> print_var e ; print_vars_list l
(**************************************Structure symbol table*)
type struc_table = {
	struc_name : string; (*name of a structure*)
	mutable struc_fields : string list; (*keys within a structure*)
}
type option_table = {
	option_name : string; (*name of a option*)
	mutable option_fields : string list; (*keys within a option*)
}
(**************************************** Symbol Tables *)
type symbol_table = { (*general symbol table for variables*)
  parent : symbol_table option;
  mutable variables : (string * Ast.dataType) list;
	mutable structs : struc_table list;
	mutable options : option_table list;
}
type environment = {
  mutable func_return_type : Ast.dataType; (* Function return type *)
  scope : symbol_table;        (* symbol table for varibles *)
  mutable functions : (string * Ast.dataType list * Ast.dataType) list; (* symbol table for global functions, nested function declaration not supported*)
}
(****************************************** Initial*)
let new_env : environment = 
  let core =[("toString", [String], String);("toString", [Int], String);
						("toString", [Float], String);("toString", [Matrix], String);
						("toString", [Structure], String);("toString", [Option], String);
						("toString", [Boolean], String);("printM", [Matrix], Void);
						("print", [String], Void);("print", [Int], String);
						("print", [Float], Void);("print", [Matrix], Void);
						("print", [Structure], Void);("print", [Option], Void);
						("print", [Boolean], Void);("toInt", [String], Int);] in 
  let s = { variables = [];structs = []; options = []; parent = None }
  in
  {scope = s ; func_return_type = Void; functions = core;}
	
let inner_scope (env : environment) : environment =
  let s = { variables = []; structs = []; options = []; parent = Some(env.scope) } in
  { env with scope = s; }
(****************************************** Utils*)
let rec check_dup l = match l with
        [] -> false
      | (h::t) ->
        let x = (List.filter (fun x -> x = h) t) in
        if (x == []) then
           check_dup t
        else
           true

let rec samelists l1 l2 = match l1, l2 with
| [], []       -> true
| [], _ | _, []        -> false
| x::xs, y::ys -> x = y && samelists xs ys

let is_keyword (var_name:string) = 
	let keyword_set = ["main";"void"] in
	try 
		List.find (fun x -> x=var_name ) keyword_set;
		true
	with Not_found -> false

let rec find_vars_exist (env_scope : symbol_table) (var_name : string) = 
  try
    List.find (fun (s, _) -> s = var_name) env_scope.variables;
    true
  with Not_found ->
		try 
			List.find (fun x ->x.struc_name =var_name) env_scope.structs;
			true
		with Not_found ->
			try
  			List.find (fun x->x.option_name = var_name) env_scope.options;
  			true
			with Not_found ->
        (match env_scope.parent with
        | Some(p) -> find_vars_exist p var_name
        | _ -> false)

let rec find_vars (env_scope : symbol_table) (var_name : string) : Ast.dataType= 
  try
    let (_, typ) = List.find (fun (s, _) -> s = var_name) env_scope.variables in
    typ
  with Not_found ->
		try 
			List.find (fun x ->x.struc_name =var_name) env_scope.structs;
			Structure
		with Not_found ->
			try
  			List.find (fun x->x.option_name = var_name) env_scope.options;
  			Option
			with Not_found ->
        (match env_scope.parent with
        | Some(p) -> find_vars p var_name
        | _ -> raise(Failure("Cannot find variable named " ^ var_name) ))

let rec find_structs (env_scope : symbol_table) (var_name : string) =
	try 
		let struct_find = List.find (fun x -> x.struc_name = var_name) env_scope.structs in
		struct_find
	with Not_found ->
		match env_scope.parent with
        | Some(p) -> find_structs p var_name
        | _ -> raise(Failure("Cannot find option named " ^ var_name) )	

let rec find_options (env_scope : symbol_table) (var_name : string) =
	try 
		let option_find = List.find (fun x -> x.option_name = var_name) env_scope.options in
		option_find
	with Not_found ->
		match env_scope.parent with
        | Some(p) -> find_options p var_name
        | _ -> raise(Failure("Cannot find option named " ^ var_name) )		
	
let find_funcs_exist (env : environment) (var_name : string) = 
	try
    List.find (fun (s, _, _) -> s = var_name) env.functions;
    true
  with Not_found -> false

let find_funcs (env : environment) (var_name : string) (arglist : Ast.dataType list)= 
	try
    let (_, _, typ) = List.find (fun (s, forlist, _) ->
																 s = var_name && samelists forlist arglist) env.functions in
    typ
  with Not_found -> raise(Failure("Cannot find function named " ^ var_name) )

let get_type (e : Sast.expr_t): Ast.dataType = 
  match e with 
   Binary_op_t(_,_,_,t) -> t
  | MatBinary_op_t(_,_,_,t) -> t
  | Id_t(_,t) -> t
  | Float_lit_t(_,t) -> t
  | Int_lit_t(_,t) -> t
  | String_lit_t(_,t) -> t
  | Call_t(_,_, t) ->t
  | VarAssign_t(_,_,t)->t
	| Matrix_element_assign_t (_,_,_,_,t) -> t
	| Struct_element_assign_t (_,_,_,t) -> t
  | Matrix_element_t(_,_,_,t)->t 
  | Precedence_expr_t(_,t)->t 
	| Struct_element_t(_,_,t)->t 
	| Bool_lit_t(_,t)->t
  | Noexpr_t(t)->t

let get_formal_type (vardec : Ast.var_dec) : Ast.dataType  = vardec.vtype

let addFormal (env: environment) (vardec : Ast.var_dec)  : unit= 
	let v_name = vardec.vname in 
	if is_keyword v_name
	 then raise(Failure("Cannot use keyword " ^ v_name ^ " as variable name"))
	else
		let exist_v = find_vars_exist env.scope v_name in (*Should allow conflict with global variables*)
		if exist_v
		  then raise(Failure("Variable name " ^ v_name ^ " already been used."))
		else 
			(env.scope.variables<-env.scope.variables@[(v_name,vardec.vtype)];
			())
(****************************************** Annotate and Check*)
let rec annotate_expr (env : environment) (e : Ast.expr): Sast.expr_t = 
  match e with 
  Binary_op(e1, op, e2) -> 
		let e1_a = annotate_expr env e1 in
		let e2_a = annotate_expr env e2
		in
			let e1_t = get_type e1_a in
			let e2_t = get_type e2_a in 
			(match op with
			| Add | Sub | Times | Divide ->
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
			| And | Or | Eq | Neq | Lt | Gt | Leq | Geq ->
				if (e1_t<>Boolean) || (e2_t<>Boolean)
					then raise(Failure("Boolean should be the types around boolean operations"))
				else Binary_op_t(e1_a, op, e2_a, e1_t)
				)
  | MatBinary_op(e1, op, e2) ->
		let e1_a = annotate_expr env e1 in
		let e2_a = annotate_expr env e2 in
			let e1_t = get_type e1_a in
			let e2_t = get_type e2_a in
			(match op with
			| MTime | MDivide |MAdd | MSub ->
				if (e1_t <> Matrix) || (e2_t <> Matrix)
				then raise(Failure("Matrix operation has to be Matrix type on both sides"))
				else MatBinary_op_t(e1_a, op, e2_a, Matrix)
			| MITime | MIDivide |MIAdd | MISub ->
				if (e1_t <> Matrix) || ((e2_t <> Int) && (e2_t <> Float))
				then raise(Failure("MatrixElment operation has to be Matrix type on left side and Integer on the right side"))
				else MatBinary_op_t(e1_a, op, e2_a, Matrix)
				)			
  | Id(s) ->
		let typ = find_vars env.scope s in Id_t(s,typ)
  | Float_lit(f) ->Float_lit_t(f,Float)
  | Int_lit(i) ->Int_lit_t(i,Int)
  | String_lit(s) ->String_lit_t(s,String)
  | Call(name, exl)->
      let exl_a = List.map (fun x -> annotate_expr env x) exl in
			let arglist = List.map (fun x -> get_type x) exl_a in
      let ret_type = find_funcs env name arglist in
				Call_t(name,exl_a,ret_type)
  | VarAssign(s, e2)->  let exp = Id(s) in annotate_assign env exp e2
	| Matrix_element_assign(s, e1, e2, e3) ->
		let exp = Matrix_element(s, e1, e2) in annotate_assign env exp e3
	| Struct_element_assign(s1, s2, e) ->
	  let exp = Struct_element(s1, s2) in annotate_assign env exp e
  | Precedence_expr(e) -> annotate_expr env e
	| Struct_element(s1,s2) -> check_struc_elem env s1 s2
	| Matrix_element(s,me1,me2) -> check_matrix_elem env s me1 me2 
	| Bool_lit(e1) -> Bool_lit_t(e1,Boolean)
  | _ -> Noexpr_t(Void)

and check_matrix_elem (env : environment) (name:string)(e1:Ast.expr)(e2:Ast.expr) : Sast.expr_t = 
	let var_type = find_vars env.scope name in
	match var_type with
	| Matrix -> 
		let e1_a = annotate_expr env e1 in
		let e2_a = annotate_expr env e2 in
		let e1_typ = get_type e1_a in
		let e2_typ = get_type e2_a in
		 if (e1_typ<>Int)||(e2_typ<>Int)
		 	then raise(Failure("Indexes of Matrix must be of Int type"))
		 else Matrix_element_t(name,e1_a,e2_a,Float)
	| _ -> raise(Failure("Should be of Matrix type for accessing elements"))

and check_struc_elem (env:environment) (name:string)(key:string) : Sast.expr_t = 
	let var_type = find_vars env.scope name in
	match var_type with
	| Structure -> 
		(let struc = find_structs env.scope name in
		try 
			List.find (fun s -> s = key) struc.struc_fields;
    	Struct_element_t(name,key,String)
  	with Not_found -> raise(Failure("Field "^key^" does not exist within struct")))
	| Option -> 
		(let opt = find_options env.scope name in
		try 
			List.find (fun s -> s = key) opt.option_fields;
    	Struct_element_t(name,key,String)
  	with Not_found -> raise(Failure("Field "^key^" does not exist within struct")))
	| _ -> raise(Failure("Should be of Structure type for accessing fields"))

and annotate_assign (env : environment) (e1 : Ast.expr) (e2 : Ast.expr) : Sast.expr_t =
  let e2_a = annotate_expr env e2 in 
  match e1 with
  | Id(x) -> 
    let e2_type = get_type e2_a in
    let e1_type = find_vars env.scope x in
    (match e1_type with
      | Float -> 
        if (e2_type = Float) || (e2_type = Int)
        then VarAssign_t(Id_t(x, e1_type), e2_a, e1_type)
        else raise(Failure("variable "^x^" need to be float type"))
      | _ -> 
        if e2_type <> e1_type
        then raise(Failure("variable "^x^" need to be assigned with same type"))
        else VarAssign_t(Id_t(x, e1_type), e2_a, e2_type)
      )
	| Matrix_element(s,me1,me2)->
		let elem_t = check_matrix_elem env s me1 me2 in
		let e2_type = get_type e2_a in
		(match e2_type with
		|Float|Int -> 
			let me1_a = annotate_expr env me1 in 
			let me2_a = annotate_expr env me2 in
			if (get_type me1_a = Int) && (get_type me2_a = Int)
				then Matrix_element_assign_t(s, me1_a, me2_a, e2_a, Float)
			else raise(Failure("Matrix indexing has to be of type int"))
		| _ -> raise(Failure("Only allow Float or Int assigned to Matrix element"))
		)
	| Struct_element(s1,s2)-> 
		check_struc_elem env s1 s2;
		let value_type = get_type e2_a in
		(match value_type with
		|String -> Struct_element_assign_t(s1,s2,e2_a,String)
		|_ ->raise(Failure("Only String type can be assigned to structure"))
		)
  | _ -> raise(Failure("Assignment need to be applied to a variable"))

let rec annotate_stmt (env : environment) (s : Ast.stmt): Sast.stmt_t = 
  match s with 
    Block(stmtlist) ->
      let env_inner = inner_scope env
      in 
        let stmt_t_list =  annotate_stmts env_inner stmtlist 
        in
          Block_t(stmt_t_list)
    | Expr(e) -> 
      let expr_a = annotate_expr env e
      in
        Expr_t(expr_a)
    | If(be, body1, body2) ->
      let be_a = annotate_expr env be in
				if (get_type be_a) <> Boolean
					then raise(Failure("there should be boolean expression within If"))
				else 
        let body1_a = annotate_stmt env body1 in
        let body2_a = annotate_stmt env body2 in
          If_t(be_a, body1_a, body2_a)
    | For(ae1, be, ae2, body)->
      let ae1_a = 
        (match ae1 with (*For other assign*)
        | VarAssign(s, e2) -> let exp = Id(s) in annotate_assign env exp e2
				| Matrix_element_assign(s, e1, e2, e3) ->
					let exp = Matrix_element(s, e1, e2) in annotate_assign env exp e3
				| Struct_element_assign(s1, s2, e) ->
	  			let exp = Struct_element(s1, s2) in annotate_assign env exp e
        | Noexpr -> Noexpr_t(Void)
        | _ -> raise(Failure("Need assignment in for loop header"))) in
      let be_a = 
        (match be with
        | Binary_op(_,_,_) -> annotate_expr env be
        | Noexpr -> Noexpr_t(Void)
				| _ -> raise((Failure("condition expression within For loop is not required")))) in
      let ae2_a = 
        (match ae2 with
        | VarAssign(s, e2) -> let exp = Id(s) in annotate_assign env exp e2
				| Matrix_element_assign(s, e1, e2, e3) ->
					let exp = Matrix_element(s, e1, e2) in annotate_assign env exp e3
				| Struct_element_assign(s1, s2, e) ->
	  			let exp = Struct_element(s1, s2) in annotate_assign env exp e
        | Noexpr -> Noexpr_t(Void)
				| _ -> raise((Failure("there should be assign expression within for loop")))) in
      let body_a = annotate_stmt env body 
        in
					if (get_type be_a<>Boolean) && (get_type be_a <> Void)
						then raise(Failure("there should be boolean expression within For"))
					else
          For_t(ae1_a, be_a, ae2_a, body_a)
    | While(be, body)  ->
      let be_a = annotate_expr env be in
      let body_a = annotate_stmt env body 
        in
				  if (get_type be_a<>Boolean) && (get_type be_a <> Void)
						then raise(Failure("there should be boolean expression within For"))
					else
          While_t(be_a, body_a)       
    | Return(e) ->
      let e_a = annotate_expr env e 
      in 
        let return_type = get_type e_a
        in
          if return_type <> env.func_return_type
          then raise(Failure("actual return type is not same with function return type: " ^ string_of_dataType env.func_return_type))
          else 
            Return_t(e_a)
    | Vardec(vd) ->
      let var_type = vd.vtype in
      let var_name = vd.vname 
      in
        if is_keyword var_name
         then raise(Failure("Cannot use keyword " ^ var_name ^ " as variable name"))
        else 
          let exist_v = find_vars_exist env.scope var_name
          in
            if exist_v 
              then raise(Failure("Variable name " ^ var_name ^ " already been used."))
            else 
							(	
								env.scope.variables <- env.scope.variables@[(var_name, var_type)];
							(*	print_vars_list env.scope.variables;*)
              Vardec_t(vd, var_type))
    | Matdec(md) ->
      let var_type = md.mtype in
      let var_name = md.mname
      in
        if is_keyword var_name
          then raise(Failure("Cannot use keyword " ^ var_name ^ "  as variable name"))
        else 
          if var_type <> Matrix
           then raise(Failure("The declaration of  " ^ var_name ^ " only apply to Matrix"))
          else
            let exist_v = find_vars_exist env.scope var_name
            in
              if exist_v 
                then raise(Failure("Variable name " ^ var_name ^ "  already been used."))
              else 
								( env.scope.variables<-env.scope.variables@[(var_name, var_type)];
                Matdec_t(md, var_type))
		| Structdec(var_name,starglist) ->
			if is_keyword var_name
          then raise(Failure("Cannot use keyword " ^ var_name ^ "  as variable name"))
      else 
          let exist_v = find_vars_exist env.scope var_name
          in
            if exist_v 
                then raise(Failure("Variable name " ^ var_name ^ "  already been used."))
            else 
							(*Check whether filed names have duplicates and type are of string*)
							let fields = List.map (fun x ->
														let fid = x.id in
														let fval = annotate_expr env x.value in
														if get_type fval <> String
															then raise(Failure("Only String can be assigned to filed of Structure"))
														else fid ) starglist 
							in 
							let dup = check_dup fields
							in
								if dup
								  then raise(Failure("There are duplicate fields inside structure "^var_name))
								else
									( env.scope.structs<-env.scope.structs@[{struc_name = var_name; struc_fields = fields}];
                Structdec_t(var_name, starglist, Structure))
	  | Optiondec(var_name,starglist) ->
			if is_keyword var_name
          then raise(Failure("Cannot use keyword " ^ var_name ^ "  as variable name"))
      else 
          let exist_v = find_vars_exist env.scope var_name
          in
            if exist_v 
                then raise(Failure("Variable name " ^ var_name ^ "  already been used."))
            else 
							(*Check whether filed names have duplicates and type are of string*)
							let fields = List.map (fun x ->
														let fid = x.id in
														let fval = annotate_expr env x.value in
														if get_type fval <> String
															then raise(Failure("Only String can be assigned to filed of Structure"))
														else fid ) starglist 
							in 
							let dup = check_dup fields
							in
								if dup
								  then raise(Failure("There are duplicate fields inside structure "^var_name))
								else
									( env.scope.options<-env.scope.options@[{option_name=var_name; option_fields=fields}];
                Optiondec_t(var_name, starglist, Option))
			
and annotate_stmts  (env : environment) (stmts : Ast.stmt list) : Sast.stmt_t list =
  List.map (fun x -> (print_string (string_of_stmt x);annotate_stmt env x)) stmts	

let annotate_global_stmts (env : environment) (stmts : Ast.stmt list) : Sast.stmt_t list = 
	List.map (fun x -> 
		(match x with
		|Vardec(_)| Matdec(_)| Structdec(_,_)| Optiondec (_,_) -> (print_string (string_of_stmt x));annotate_stmt env x
		| _ -> raise(Failure("Global statements can only be variable declaration")))
		) stmts	

let annotate_func (env: environment) (func : Ast.func_dec) = 
	let ret_type = func.ret in
	let name = func.func_name in
  	if is_keyword name && name <> "main"
          then raise(Failure("Cannot use keyword " ^ name ^ " as function name"))
    else 
        let exist_f = find_funcs_exist env name
            in
              if exist_f 
                then raise(Failure("Function name " ^ name ^ " already been used."))
              else 
								let inner_env = inner_scope env in
								 inner_env.func_return_type <- ret_type;
  							 let formals_list = func.formals in
								  List.iter (fun x -> addFormal inner_env x) formals_list;
								    let body_stmts = func.body in
								      annotate_stmts inner_env body_stmts;
								        let data_type_list = List.map (fun x-> get_formal_type x) formals_list in
								          env.functions <- env.functions @ [(name,data_type_list,ret_type)]

let annotate_funcs  (env : environment) (funcs : Ast.func_dec list)(*: Sast.func_dec_t list*)  = 
	List.map (fun x -> annotate_func env x) funcs
	
(****************************************** Checking Program and main function exists *)
let check_program (stmts, funcs) = 
	let globals = List.rev stmts in
	let functions = List.rev funcs in
	let env = new_env in
    annotate_global_stmts env globals;
    annotate_funcs env functions;
		let main_exist = find_funcs_exist env "main" in
		if main_exist
		  then print_endline "\nSemantic analysis completed successfully."
		else
			raise(Failure("Must declare main function"))
