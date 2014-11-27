open Sast
open Printf
open Random
open Ast

(************
  HELPERS
************)
(**
expr_t =
	Binary_op_t of expr_t * bin_op * expr_t * dataType
  | MatBinary_op_t of expr_t * mat_op * expr_t * dataType
  | Id_t of string * dataType
  | Float_lit_t of float * dataType
  | Int_lit_t of int * dataType
  | String_lit_t of string * dataType
  | Boolean_t of string * dataType
  | Call_t of string * expr_t list * dataType
  | VarAssign_t of expr_t * expr_t * dataType
  | Matrix_element of string * expr_t * expr_t * dataType
  | Precedence_expr_t of expr_t * dataType
	| Struct_element_t of string * string * dataType
  | Noexpr_t of dataType
**)
let type_of (ae : Sast.expr_t) : Ast.dataType =
  match ae with
  | Binary_op_t(_, _, _, t) -> t
  | MatBinary_op_t (_, _, _, t) -> t
	| Id_t (_, t) -> t
  | Float_lit_t(_, t) -> t
  | Int_lit_t(_, t) -> t
  | String_lit_t( _, t) -> t
  | Boolean_t(_, t) -> t
  | Call_t(_, _, t) -> t
  | VarAssign_t(_, _, t) -> t
  | Matrix_element(_,_,_, t) -> t
  | Precedence_expr_t(_, t) -> t
  | Struct_element_t(_, _, t) -> t
  | Noexpr_t( t) -> t
(*   Int -> "int"
	| Float -> "float"
	| String -> "string"
	| Matrix -> "matrix"
	| Option -> "option"
	| Structure -> "structure"
	| Boolean -> "bool"
	| Void -> "void"
*)
let java_from_type (ty: Ast.dataType) : string = 
    match ty with
      | Int -> "int" 
      | Float -> "float"
			| String -> "string"
			| Matrix -> "Matrix"
			| Option -> "Option"
			| Structure -> "Structure"
			| Boolean -> "boolean"
			| Void -> "void"



(*******************************************************************************
  Expression and statement evaluation
********************************************************************************)

let rec writeToFile fileName progString = 
  let file = open_out ("java/" ^ fileName ^ ".java") in
    fprintf file "%s"  progString

and gen_program fileName prog = (*have a writetofile*)
  let stmtString = writeStmtList prog in
  let out = sprintf "
  public class %s
  {
      public static void main(String[] args)
      {
          DistributeClient.setSlaves(args);
%s
      } 
  }
  " fileName stmtString in
  writeToFile fileName out;
  out

and writeStmtList stmtList = 
  let outStr = List.fold_left (fun a b -> a ^ (gen_stmt b)) "" stmtList in
  	sprintf "%s" outStr
		(*    type stmt_t =*)
		(* Block_t of stmt_t list
  | Expr_t of expr_t 
  | If_t of expr_t * stmt_t * stmt_t 
  | For_t of expr_t * expr_t * expr_t * stmt_t
  | While_t of expr_t * stmt_t 
  | Return_t of expr_t
  | Vardec_t of var_dec * dataType
  | Matdec_t of mat_dec * dataType
	| Structdec_t of string * expr_t list * dataType
	| Optiondec_t of string * expr_t list * dataType
	*)
and gen_stmt = function
		Block_t(stmtList) -> "\n{\n"^writeStmtList stmtList^"\n}\n"
	| Expr_t(expr_t) -> gen_expr expr_t ^";\n"
  | If_t ( expr_t , stmt_t_If , stmt_t_Else) -> "if ("^gen_expr expr_t^") {"^ gen_stmt stmt_t_If
	^"}else{" ^ gen_stmt stmt_t_Else^"}\n"
  | For_t(expr_t1 , expr_t2 , expr_t3 , stmt_t)-> 
      "for (" ^ gen_expr expr_t1 ^ "," ^ gen_expr expr_t2^","
			^ gen_expr expr_t3^")\n{\n" ^ gen_stmt stmt_t^"\n}\n"
  | While_t ( expr_t , stmt_t ) -> "while("^gen_expr expr_t^")\n{\n"^gen_stmt stmt_t^"\n}\n"
	| Return_t (expr_t)  -> "return "^gen_expr expr_t^";\n"
	| Vardec_t (var_dec , dataType) ->  java_from_type var_dec.vtype ^" "^var_dec.vname^";\n"
  | Matdec_t (mat_dec , dataType) -> java_from_type mat_dec.mtype ^" "^mat_dec.mname^" = new "
	 ^java_from_type mat_dec.mtype^"("^mat_dec.mrow^","^mat_dec.mcol^");\n"
	| Structdec_t (structName , exprList) -> java_from_type Structure ^" "^mat_dec.mname^" = new "
	 ^java_from_type Structure^"();\n"^ writeExprList exprList^"\n"
	(*type expr_t =
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
  | Noexpr_t of dataType*)
and gen_expr = function
    Id_t(flt, _)   -> flt
  | Float_lit_t(floatLit, _) -> writeBoolLit boolLit
  | ACharLit(charLit, _) -> writeCharLit charLit
  | AId(name, typed, typ) -> writeID  name typed 
  | AFuncCreate(params, body, _) -> writeFunc params body
  | AFuncCallExpr(exp, params, ty) -> writeFuncCall exp params (Some(ty))
  | AObjAccess(objName, fieldName, ty)-> writeObjectAccess objName fieldName ty
  | AListAccess(listName, idx, ty) -> writeListAccess listName idx ty
  | AListCreate(contentList,ty) -> writeListCreate contentList ty
  | ASublist(listName, leftIdx, rightIdx, _) -> 
      writeSubList listName leftIdx rightIdx
  | AObjCreate(nVTplList, _) ->  writeObjCreate nVTplList
  | ABinop(ope1, op, ope2, _) -> writeBinop ope1 op ope2
  | ANot(exp, _) -> writeUnaryNot exp



(*******************************************************************************
  Specific Statement evaluation
********************************************************************************)
and writeStmtList stmtList = 
    sprintf "%s" ("{\n  " ^ String.concat "  " (List.map gen_stmt stmtList) ^ "}\n")

and writeExprList exprList = 
    sprintf "%s" ("\n /* " ^ String.concat "  " (List.map gen_expr exprList) ^ "\n*/")


and writeReturnStmt exp = 
  let expStr = (gen_expr exp) in
    sprintf "return %s;" expStr

and writeIfStmt condTupleList elseStmtList = 
  let string_of_tuple (condExpr, stmtList) =
    let body = writeStmtList stmtList
    and cond = gen_expr condExpr in
    sprintf "
    if (%s.<Boolean>getBase())
    {
        %s\t}" cond body
  in let ifString =
    let rec string_of_tupleList = function
        [] -> ""
      | a::[] -> string_of_tuple a ^ "\n"
      | a::tl -> (string_of_tuple a) ^ " else " ^ string_of_tupleList tl
    in string_of_tupleList condTupleList 
  and elseString = 
    let checkForNone str = match str with 
        Some(str) -> "else{\n" ^ writeStmtList str ^ "\n}"
      | None -> ""
    in checkForNone elseStmtList 
in sprintf "%s\n%s" ifString elseString

and writeForLoop asnTuple cond incrTuple stmtList =
  let asn = 
    let matchTuple = function
        Some(asnTup) ->   gen_expr (fst asnTup) ^ "=" ^ gen_expr (snd asnTup)
      | None -> ""
    in matchTuple asnTuple
  and stmtString = writeStmtList stmtList  
  and cond = 
    let matchCond = function
        Some(cond) -> gen_expr cond
      | None -> ""
    in matchCond cond 
  and incrString = 
    let matchTuple = function
        Some(incrTup) ->   gen_expr (fst incrTup) ^ "=" ^ gen_expr (snd incrTup)
      | None -> ""
    in matchTuple incrTuple
  in
  sprintf "for (%s;%s.<Boolean>getBase();%s)\n{\n%s\n}\n" asn cond incrString stmtString

and writeWhileLoop cond stmtList =
  let condString = gen_expr cond 
  and stmtString = writeStmtList stmtList in 
    sprintf "while (%s.<Boolean>getBase())\n{\n%s\n}\n" condString stmtString

(*ASSIGNING IS SPECIAL SO WE HANDWROTE THESE WITH LOVE*)
and writeAssign expr1 expr2 =
    let lhs_type = java_from_type (type_of expr2) in
    let e2string = gen_expr expr2 in
      match expr1 with
        | AId(name, typed, typ) -> 
          if typed
          then
            sprintf "%s %s = (%s)(%s);\n" lhs_type name lhs_type e2string
          else
            sprintf "%s = (%s)(%s);\n" name lhs_type e2string
        | AListAccess(listName, idx, _) -> 
          let listNamestring = gen_expr listName and idxstring = gen_expr idx in
          sprintf "%s.set(%s, %s);\n" listNamestring idxstring e2string
        | AObjAccess(objName, fieldName, _)->
          let objNamestring = gen_expr objName in 
          sprintf "%s.set(\"%s\", %s);\n" objNamestring fieldName e2string
        | _ -> failwith "How'd we get all the way to java with this!!!! Not a valid LHS"

and writeFuncCallStmt fNameExpr paramsListExpr = 
  (writeFuncCall fNameExpr paramsListExpr None) ^ ";\n"



(*******************************************************************************  
    Function handling - helper functions
********************************************************************************)

and writeFunc params stmtList = 
  let fName = function_name_gen() in
  let fileName = "java/" ^ fName ^ ".java" in 
  let file = open_out fileName in
  let paramSetting = snd (List.fold_left (
                            fun a p -> 
                              let count = fst a in
                              let sofar = snd a in
                              let typeName = java_from_type (snd p) in 
                              let newString = typeName ^" " ^ (fst p) ^
                              " = (" ^ typeName ^ ")args[" ^ string_of_int count ^ "];\n"
                              in
                              (count +1, sofar ^ newString)
                          ) (0, "") params)
  and body = writeStmtList stmtList in
  fprintf file "
  import java.io.Serializable; 
  public class %s extends IPCFunction implements Serializable
  {
      public %s() {}

      public PCObject call(PCObject... args)
      {
          %s
          %s
      }
  }
  " fName fName paramSetting body;
  sprintf "new %s()" fName

and writeFuncCall toCallExp paramsExp optCast =
  let toCall = (gen_expr toCallExp) and params = (params_to_string paramsExp) in 
  match optCast with
  | Some(ty) -> sprintf "((%s)%s.call(%s))" (java_from_type ty) toCall params
  | None -> sprintf "%s.call(%s)" toCall params

and params_to_string paramsList= 
  let paramsStringList = List.map gen_expr paramsList in
    let rec paramsListtoString = function
        [] -> ""
      | [a] -> sprintf("%s") a 
      | hd::tl -> (sprintf("%s,") hd)^paramsListtoString tl
    in paramsListtoString paramsStringList 

and pNames_to_string paramTupleList = 
  let pNameList = List.map (fun (a,b) -> a) paramTupleList in
  List.fold_left (fun a b -> a^b) "" pNameList


  

(*******************************************************************************  
    List and Object handling - helper functions
********************************************************************************)

and writeObjectAccess objNameExpr fieldName ty=
  let objName = gen_expr objNameExpr in 
  let access_type_string = java_from_type ty in
  sprintf "%s.<%s>get(\"%s\")" objName access_type_string fieldName

and writeListAccess listNameExpr idxExpr ty=
  let listName = gen_expr listNameExpr and idx = gen_expr idxExpr in
  let access_type_string = java_from_type ty in
  sprintf "%s.<%s>get(%s)" listName access_type_string idx

and writeListCreate exprList ty =
  match ty with 
    | TList(t) -> 
      (match t with
        | TChar -> 
            let implode l =
              let res = String.create (List.length l) in
              let rec imp i = function
                | [] -> res
                | c :: l -> res.[i] <- c; imp (i + 1) l
              in
              imp 0 l
            in
            let char_from_expr = function
              | ACharLit(c, _) -> c
              | _ -> failwith "NEVER: list of characters has non-character"
            in
            let word = implode (List.map char_from_expr exprList) in
            sprintf "new PCList(\"%s\")" word
        | _ -> 
            let concatAdds = (fun a b -> a^(sprintf(".add(%s)") b)) 
            and list_of_strings = List.map gen_expr exprList in 
            List.fold_left concatAdds "new PCList()" list_of_strings
      )
    | _ -> failwith "NEVER: trying to generate a list from a non list type"
    
and writeSubList listNameExpr startIdxExpr endIdxExpr = 
  let listName = gen_expr listNameExpr in  
  let startIdx = 
    let det = function
        Some(x) -> gen_expr x
      | None -> "new PCObject(0)"
    in det startIdxExpr 
  and endIdx = 
    let det = function
        Some(x) -> gen_expr x
      | None -> sprintf "new PCObject(%s.size()-1)" listName 
    in det endIdxExpr 
  in sprintf "%s.subList(%s,%s)" listName startIdx endIdx

and writeObjCreate kvt_list = 
  let string_of_tuple (k , vExpr)  = 
    let v = gen_expr vExpr in
    sprintf ".set(\"%s\", %s)" k v
  in let stringList = List.map string_of_tuple kvt_list; in 
    List.fold_left (fun a b -> a^b) "new PCObject()" stringList



(*******************************************************************************  
    Binop and Not Handling - helper functions
********************************************************************************)

and writeUnaryNot boolExpr = 
  let boolObj = gen_expr boolExpr in
  sprintf "!%s" boolObj

and writeBinop expr1 op expr2 = 
  let e1 = gen_expr expr1 and e2 = gen_expr expr2 in
    let writeBinopHelper e1 op e2 = match op with
        Add  -> sprintf "new PCObject(%s.<Double>getBase() + %s.<Double>getBase())" e1 e2
      | Sub  -> sprintf "new PCObject(%s.<Double>getBase() - %s.<Double>getBase())" e1 e2  
      | Mult -> sprintf "new PCObject(%s.<Double>getBase() * %s.<Double>getBase())" e1 e2
      | Div  -> sprintf "new PCObject(%s.<Double>getBase() / %s.<Double>getBase())" e1 e2
      | Mod -> sprintf "new PCObject(%s.<Double>getBase() %% %s.<Double>getBase())" e1 e2
      | Less -> sprintf "new PCObject(%s.<Double>getBase() < %s.<Double>getBase())" e1 e2
      | Leq -> sprintf "new PCObject(%s.<Double>getBase() <= %s.<Double>getBase())" e1 e2   
      | Greater -> sprintf "new PCObject(%s.<Double>getBase() > %s.<Double>getBase())" e1 e2 
      | Geq -> sprintf "new PCObject(%s.<Double>getBase() >= %s.<Double>getBase())" e1 e2
      | And -> sprintf "new PCObject(%s.<Boolean>getBase() && %s.<Boolean>getBase())" e1 e2    
      | Or -> sprintf "new PCObject(%s.<Boolean>getBase() || %s.<Boolean>getBase())" e1 e2 
      | Equal -> sprintf "new PCObject(%s.equals(%s))" e1 e2
      | Neq -> sprintf "new PCObject(!(%s.equals(%s)))" e1 e2
      | Concat -> sprintf "new PCList(%s,%s)" e1 e2
    in writeBinopHelper e1 op e2


(*******************************************************************************  
    Id handling - helper function
********************************************************************************)

and writeID idName ty =
  let newName = (match idName with
  | "rec" -> "this"
  | "print" | "printFile" | "read" | "readFile" | "download" | "numToString" | "numFromString"  ->
      sprintf "IO.get(\"%s\")" idName
  | "distribute" -> "DistributeClient.distribute"
  | _ -> idName) in
  match ty with 
    true -> sprintf "PCObject %s" newName
  | false -> sprintf "%s" newName


(*******************************************************************************  
    Literal expression handling - helper functions
********************************************************************************)

and writeNumLit numLit = 
  if floor(numLit) = numLit
  then sprintf "new PCObject(%d)" (int_of_float numLit)
  else sprintf "new PCObject(%f)" numLit

and writeBoolLit boolLit = 
  sprintf "new PCObject(%b)" boolLit

and writeCharLit charLit =
  let fChar = Char.escaped charLit in 
  sprintf "new PCObject('%s')" fChar


(*******************************************************************************  
    Function Name Generation - helper functions
********************************************************************************)

and function_name_gen() = 
  Random.self_init();
  let x = (Random.int 100000) in 
  sprintf "function_%d" x
