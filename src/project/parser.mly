%{ open Ast %}

%token SEMI COLON LPAREN RPAREN LSQUARE RSQUARE LBRACE RBRACE COMMA
%token PLUS MINUS MPLUS MMINUS MIPLUS MIMINUS TIMES DIVIDE MTIMES MDIVIDE MITIMES MIDIVIDE ASSIGN ARROW
%token EQ NEQ LT LEQ GT GEQ AND OR
%token RETURN IF ELSE FOR WHILE
%token BOOLEAN TRUE FALSE MATRIX STRUCTURE OPTION INT FLOAT STRING VOID
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <string> STRING_LIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE COMMA
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS MPLUS MMINUS MIPLUS MIMINUS
%left TIMES DIVIDE MTIMES MDIVIDE MITIMES MIDIVIDE
%left ARROW

%start program
%type <Ast.program> program

%%

/*
program:
  { {gvdecls=[]; gfdecls=[]} }
 | program vdecl { {gvdecls=($2 :: $1.gvdecls); gfdecls=$1.gfdecls} }
 | program fdecl { {gvdecls=$1.gvdecls; gfdecls=($2 :: $1.gfdecls)} }
*/
program:
   { [], [] }
 | program stmt {($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   retval formal_list RPAREN LBRACE stmt_list RBRACE
     { { func_name =  $1.vname;
         formals = List.rev $2; 
         body = List.rev $5;
         ret =  $1.vtype
         } }

retval:
        INT ID LPAREN { {vtype=Int; vname=$2}  }
        |FLOAT ID LPAREN { {vtype=Float;vname= $2}  }
        |VOID ID LPAREN { {vtype=Void; vname=$2 } }
				|MATRIX ID LPAREN { {vtype=Matrix; vname=$2}  }
				| OPTION ID LPAREN { {vtype=Option;vname= $2}  }
				| STRUCTURE ID LPAREN { {vtype=Structure;vname= $2}  }
				| BOOLEAN ID LPAREN { {vtype=Boolean; vname=$2}  }
				| STRING ID LPAREN { {vtype=String;vname= $2}  }

/*formals_opt:
     { [] }
  | formal_list   { List.rev $1 }
*/
formal_list:
    formal                   { [$1] }
  | formal_list COMMA formal { $3 :: $1 }

formal:
		tdecl      { $1 }
	| MATRIX ID  { {vtype=Matrix; vname=$2}  }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    tdecl SEMI { $1 }

tdecl:
    INT ID { {vname = $2; vtype = Int}  }
	| FLOAT ID  { {vname = $2; vtype = Float}  }
	| VOID ID  { {vname = $2; vtype = Void}  }
	| OPTION ID  { {vname = $2; vtype = Option}  }
	| STRUCTURE ID  { {vname = $2; vtype = Structure}  }
	| BOOLEAN ID  { {vname = $2; vtype = Boolean}  }
	| STRING ID  { {vname = $2; vtype = String}  }

mdecl:
		MATRIX ID LPAREN INT_LIT COMMA INT_LIT RPAREN { {mname = $2; mtype = Matrix; mrow = $4; mcol = $6}  }	

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

struct_arg:
  ID ASSIGN expr { {id = $1; value = $3} }

struct_arg_list:
    /* nothing */  { [] }
	|	struct_arg	{ [$1] }
	| struct_arg_list COMMA struct_arg {$3 :: $1}

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
	| RETURN SEMI {Return(Noexpr)}
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
	| tdecl SEMI { Vardec($1) }
	| mdecl SEMI { Matdec($1) }
	| STRUCTURE ID ASSIGN LBRACE struct_arg_list RBRACE SEMI {Structdec($2, $5)}
	| OPTION ID ASSIGN LBRACE struct_arg_list RBRACE SEMI {Optiondec($2, $5)}

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INT_LIT          { Int_lit($1) }
	| FLOAT_LIT 				{Float_lit($1)}
	| STRING_LIT 				{String_lit($1)}
	|	TRUE         { Bool_lit(1) }
	| FALSE        { Bool_lit(0) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binary_op($1, Add,   $3) }
  | expr MINUS  expr { Binary_op($1, Sub,   $3) }
  | expr TIMES  expr { Binary_op($1, Times,  $3) }
  | expr DIVIDE expr { Binary_op($1, Divide,   $3) }
	| expr MPLUS   expr { MatBinary_op($1, MAdd,   $3) }
  | expr MMINUS  expr { MatBinary_op($1, MSub,   $3) }
  | expr MTIMES  expr { MatBinary_op($1, MTime,  $3) }
  | expr MDIVIDE expr { MatBinary_op($1, MDivide,   $3) }
	| expr MIPLUS   expr { MatBinary_op($1, MIAdd,   $3) }
  | expr MIMINUS  expr { MatBinary_op($1, MISub,   $3) }
  | expr MITIMES  expr { MatBinary_op($1, MITime,  $3) }
  | expr MIDIVIDE expr { MatBinary_op($1, MIDivide,   $3) }
  | ID ASSIGN expr   { VarAssign($1, $3) }
	| ID LSQUARE expr RSQUARE LSQUARE  expr RSQUARE ASSIGN expr   { Matrix_element_assign($1, $3, $6, $9) }
	| ID ARROW ID ASSIGN expr { Struct_element_assign($1, $3, $5) }
	| ID LSQUARE expr RSQUARE LSQUARE  expr RSQUARE  { Matrix_element($1, $3, $6) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { Precedence_expr($2) }
	| ID ARROW ID {Struct_element($1, $3)}
/* b_expr: */
  | expr EQ     expr { Binary_op($1, Eq, $3) }
  | expr NEQ    expr { Binary_op($1, Neq,   $3) }
  | expr LT     expr { Binary_op($1, Lt,  $3) }
  | expr LEQ    expr { Binary_op($1, Leq,   $3) }
  | expr GT     expr { Binary_op($1, Gt,  $3) }
  | expr GEQ    expr { Binary_op($1, Geq,   $3) }
	| expr AND    expr { Binary_op($1, And,   $3) }
	| expr OR     expr { Binary_op($1, Or,   $3) }


actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
