%{ open Ast %}

%token SEMI COLON LPAREN RPAREN LSQUARE RSQUARE LBRACE RBRACE COMMA
%token PLUS MINUS MPLUS MMINUS MTIMES TIMES DIVIDE MDIVIDE ASSIGN ARROW
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
%left EQ NEQ AND OR
%left LT GT LEQ GEQ
%left PLUS MINUS MPLUS MMINUS
%left TIMES DIVIDE MTIMES MDIVIDE
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
 | program vdecl {($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   retval formal_list RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { func_name =  $1.vname;
         formals = $2; 
         locals = List.rev $5;
         body = List.rev $6;
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
    tdecl                   { [$1] }
  | formal_list COMMA tdecl { $3 :: $1 }
vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }
vdecl:
   | tdecl SEMI { $1 }
tdecl:
     INT ID { {vname = $2; vtype = Int}  }
        |FLOAT ID  { {vname = $2; vtype = Float}  }
        |VOID ID  { {vname = $2; vtype = Void}  }
				|MATRIX ID  { {vname = $2; vtype = Matrix}  }
				| OPTION ID  { {vname = $2; vtype = Option}  }
				| STRUCTURE ID  { {vname = $2; vtype = Structure}  }
				| BOOLEAN ID  { {vname = $2; vtype = Boolean}  }
				| STRING ID  { {vname = $2; vtype = String}  }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
	| RETURN SEMI {Return(Noexpr)}
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN b_expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN b_expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI b_expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN b_expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INT_LIT          { Int_lit($1) }
	| FLOAT_LIT 				{Float_lit($1)}
	| STRING_LIT 				{String_lit($1)}
  | ID               { Id($1) }
  | expr PLUS   expr { Binary_op($1, Add,   $3) }
  | expr MINUS  expr { Binary_op($1, Sub,   $3) }
  | expr TIMES  expr { Binary_op($1, Times,  $3) }
  | expr DIVIDE expr { Binary_op($1, Divide,   $3) }
	| expr MPLUS   expr { MatBinary_op($1, MAdd,   $3) }
  | expr MMINUS  expr { MatBinary_op($1, MSub,   $3) }
  | expr MTIMES  expr { MatBinary_op($1, MTime,  $3) }
  | expr MDIVIDE expr { MatBinary_op($1, MDivide,   $3) }
  | expr ASSIGN expr   { VarAssign($1, $3) }
	| ID LSQUARE expr RSQUARE LSQUARE  expr RSQUARE ASSIGN expr { ElemAssign($1, $3, $6, $9) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
b_expr:
   expr EQ     expr { Bool_expr1($1, Eq, $3) }
  | expr NEQ    expr { Bool_expr1($1, Neq,   $3) }
  | expr LT     expr { Bool_expr1($1, Lt,  $3) }
  | expr LEQ    expr { Bool_expr1($1, Leq,   $3) }
  | expr GT     expr { Bool_expr1($1, Gt,  $3) }
  | expr GEQ    expr { Bool_expr1($1, Geq,   $3) }
	| b_expr AND    b_expr { Bool_expr2($1, And,   $3) }
	| b_expr OR    b_expr { Bool_expr2($1, Or,   $3) }


actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
