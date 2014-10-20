type token =
  | SEMI
  | COLON
  | LPAREN
  | RPAREN
  | LSQUARE
  | RSQUARE
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | MPLUS
  | MMINUS
  | MTIMES
  | TIMES
  | DIVIDE
  | MDIVIDE
  | ASSIGN
  | ARROW
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | BOOLEAN
  | TRUE
  | FALSE
  | MATRIX
  | STRUCTURE
  | OPTION
  | INT
  | FLOAT
  | STRING
  | VOID
  | INT_LIT of (int)
  | FLOAT_LIT of (float)
  | STRING_LIT of (string)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 55 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* COLON *);
  259 (* LPAREN *);
  260 (* RPAREN *);
  261 (* LSQUARE *);
  262 (* RSQUARE *);
  263 (* LBRACE *);
  264 (* RBRACE *);
  265 (* COMMA *);
  266 (* PLUS *);
  267 (* MINUS *);
  268 (* MPLUS *);
  269 (* MMINUS *);
  270 (* MTIMES *);
  271 (* TIMES *);
  272 (* DIVIDE *);
  273 (* MDIVIDE *);
  274 (* ASSIGN *);
  275 (* ARROW *);
  276 (* EQ *);
  277 (* NEQ *);
  278 (* LT *);
  279 (* LEQ *);
  280 (* GT *);
  281 (* GEQ *);
  282 (* AND *);
  283 (* OR *);
  284 (* RETURN *);
  285 (* IF *);
  286 (* ELSE *);
  287 (* FOR *);
  288 (* WHILE *);
  289 (* BOOLEAN *);
  290 (* TRUE *);
  291 (* FALSE *);
  292 (* MATRIX *);
  293 (* STRUCTURE *);
  294 (* OPTION *);
  295 (* INT *);
  296 (* FLOAT *);
  297 (* STRING *);
  298 (* VOID *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  299 (* INT_LIT *);
  300 (* FLOAT_LIT *);
  301 (* STRING_LIT *);
  302 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\005\000\005\000\006\000\006\000\
\002\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\007\000\007\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\012\000\012\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\013\000\013\000\014\000\
\014\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\007\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\001\000\003\000\000\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\000\000\002\000\002\000\003\000\002\000\003\000\005\000\
\007\000\009\000\005\000\000\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\009\000\004\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\003\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\017\000\011\000\008\000\010\000\009\000\005\000\006\000\
\012\000\007\000\024\000\021\000\023\000\022\000\018\000\019\000\
\025\000\020\000\000\000\000\000\015\000\014\000\000\000\016\000\
\000\000\000\000\026\000\004\000\000\000\000\000\000\000\000\000\
\038\000\039\000\040\000\000\000\027\000\000\000\000\000\000\000\
\030\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\053\000\031\000\029\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\048\000\044\000\045\000\049\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\052\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\060\000\061\000\000\000\035\000\000\000\
\000\000\000\000\000\000\000\000\033\000\000\000\000\000\000\000\
\000\000\034\000\000\000"

let yydgoto = "\002\000\
\003\000\012\000\013\000\014\000\032\000\055\000\057\000\015\000\
\069\000\070\000\094\000\096\000\099\000\100\000"

let yysindex = "\004\000\
\000\000\000\000\111\000\216\254\219\254\224\254\226\254\233\254\
\250\254\252\254\253\254\000\000\000\000\121\000\051\255\067\255\
\069\255\077\255\083\255\087\255\090\255\096\255\110\255\068\255\
\070\255\071\255\086\255\107\255\108\255\114\255\117\255\055\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\116\255\121\000\000\000\000\000\121\000\000\000\
\005\255\032\255\000\000\000\000\001\255\139\255\163\255\173\255\
\000\000\000\000\000\000\115\255\000\000\064\000\102\000\060\255\
\000\000\072\000\032\255\032\255\032\255\032\255\032\255\032\255\
\000\000\032\255\032\255\032\255\032\255\032\255\032\255\032\255\
\032\255\000\000\000\000\000\000\080\000\081\255\154\000\160\255\
\161\255\154\000\186\255\168\255\114\000\154\000\039\000\039\000\
\039\000\039\000\000\000\000\000\000\000\000\000\032\255\032\255\
\032\255\032\255\032\255\032\255\112\255\032\255\032\255\032\255\
\112\255\000\000\032\255\187\255\154\000\154\000\154\000\154\000\
\154\000\154\000\169\255\000\000\000\000\101\255\000\000\154\000\
\032\255\112\255\032\255\126\000\000\000\189\255\182\255\112\255\
\032\255\000\000\154\000"

let yyrindex = "\000\000\
\000\000\000\000\202\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\202\255\
\213\255\214\255\216\255\218\255\219\255\228\255\229\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\066\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\158\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\240\255\000\000\238\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\006\255\000\000\
\000\000\062\255\000\000\242\255\000\000\125\255\185\255\212\255\
\239\255\010\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\200\255\227\255\254\255\025\000\
\041\000\043\000\093\255\000\000\000\000\000\000\000\000\126\255\
\000\000\000\000\252\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\037\000"

let yygindex = "\000\000\
\000\000\189\000\000\000\000\000\000\000\000\000\188\000\243\255\
\150\255\198\255\220\255\118\000\000\000\000\000"

let yytablesize = 427
let yytable = "\071\000\
\033\000\073\000\074\000\058\000\001\000\016\000\037\000\058\000\
\017\000\037\000\131\000\059\000\060\000\018\000\135\000\019\000\
\093\000\095\000\093\000\098\000\101\000\102\000\020\000\103\000\
\104\000\105\000\106\000\107\000\108\000\109\000\110\000\141\000\
\061\000\062\000\058\000\063\000\064\000\146\000\054\000\021\000\
\097\000\022\000\023\000\065\000\066\000\067\000\068\000\065\000\
\066\000\067\000\068\000\034\000\125\000\126\000\127\000\128\000\
\129\000\130\000\051\000\093\000\093\000\093\000\058\000\052\000\
\136\000\064\000\059\000\091\000\026\000\035\000\064\000\036\000\
\026\000\026\000\065\000\066\000\067\000\068\000\140\000\037\000\
\095\000\132\000\133\000\134\000\117\000\038\000\147\000\061\000\
\062\000\039\000\063\000\064\000\040\000\026\000\026\000\032\000\
\026\000\026\000\041\000\032\000\032\000\139\000\065\000\066\000\
\067\000\068\000\118\000\119\000\026\000\026\000\026\000\026\000\
\042\000\043\000\058\000\044\000\045\000\078\000\059\000\079\000\
\032\000\032\000\053\000\032\000\032\000\050\000\118\000\119\000\
\050\000\065\000\050\000\046\000\080\000\050\000\065\000\032\000\
\032\000\032\000\032\000\061\000\062\000\075\000\063\000\064\000\
\050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
\047\000\048\000\065\000\066\000\067\000\068\000\041\000\049\000\
\120\000\041\000\050\000\041\000\121\000\076\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\077\000\
\123\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\042\000\118\000\119\000\042\000\122\000\042\000\137\000\
\144\000\042\000\042\000\042\000\042\000\042\000\138\000\145\000\
\054\000\066\000\024\000\054\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\043\000\021\000\023\000\043\000\
\022\000\043\000\018\000\019\000\043\000\043\000\043\000\043\000\
\043\000\054\000\054\000\055\000\025\000\020\000\055\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\043\000\046\000\
\036\000\062\000\046\000\056\000\046\000\063\000\072\000\046\000\
\046\000\046\000\046\000\046\000\055\000\055\000\056\000\036\000\
\142\000\056\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\046\000\047\000\000\000\000\000\047\000\000\000\047\000\
\000\000\000\000\047\000\047\000\047\000\047\000\047\000\056\000\
\056\000\057\000\000\000\000\000\057\000\047\000\047\000\047\000\
\047\000\047\000\047\000\047\000\047\000\051\000\000\000\000\000\
\051\000\058\000\051\000\059\000\058\000\051\000\059\000\000\000\
\000\000\000\000\057\000\057\000\086\000\087\000\088\000\089\000\
\051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
\081\000\000\000\058\000\058\000\059\000\059\000\000\000\000\000\
\092\000\082\000\083\000\084\000\085\000\086\000\087\000\088\000\
\089\000\082\000\083\000\084\000\085\000\086\000\087\000\088\000\
\089\000\082\000\083\000\084\000\085\000\086\000\087\000\088\000\
\089\000\000\000\000\000\111\000\112\000\113\000\114\000\115\000\
\116\000\090\000\000\000\000\000\000\000\000\000\000\000\082\000\
\083\000\084\000\085\000\086\000\087\000\088\000\089\000\124\000\
\000\000\000\000\000\000\082\000\083\000\084\000\085\000\086\000\
\087\000\088\000\089\000\143\000\000\000\000\000\000\000\082\000\
\083\000\084\000\085\000\086\000\087\000\088\000\089\000\004\000\
\000\000\000\000\005\000\006\000\007\000\008\000\009\000\010\000\
\011\000\024\000\000\000\000\000\025\000\026\000\027\000\028\000\
\029\000\030\000\031\000\082\000\083\000\084\000\085\000\086\000\
\087\000\088\000\089\000"

let yycheck = "\058\000\
\014\000\001\001\061\000\003\001\001\000\046\001\001\001\003\001\
\046\001\004\001\117\000\007\001\008\001\046\001\121\000\046\001\
\075\000\076\000\077\000\078\000\079\000\080\000\046\001\082\000\
\083\000\084\000\085\000\086\000\087\000\088\000\089\000\138\000\
\028\001\029\001\003\001\031\001\032\001\144\000\052\000\046\001\
\077\000\046\001\046\001\043\001\044\001\045\001\046\001\043\001\
\044\001\045\001\046\001\001\001\111\000\112\000\113\000\114\000\
\115\000\116\000\004\001\118\000\119\000\120\000\003\001\009\001\
\123\000\004\001\007\001\008\001\003\001\003\001\009\001\003\001\
\007\001\008\001\043\001\044\001\045\001\046\001\137\000\003\001\
\139\000\118\000\119\000\120\000\004\001\003\001\145\000\028\001\
\029\001\003\001\031\001\032\001\003\001\028\001\029\001\003\001\
\031\001\032\001\003\001\007\001\008\001\001\001\043\001\044\001\
\045\001\046\001\026\001\027\001\043\001\044\001\045\001\046\001\
\003\001\046\001\003\001\046\001\046\001\003\001\007\001\005\001\
\028\001\029\001\007\001\031\001\032\001\001\001\026\001\027\001\
\004\001\004\001\006\001\046\001\018\001\009\001\009\001\043\001\
\044\001\045\001\046\001\028\001\029\001\003\001\031\001\032\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\046\001\046\001\043\001\044\001\045\001\046\001\001\001\046\001\
\001\001\004\001\046\001\006\001\004\001\003\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\003\001\
\009\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\001\001\026\001\027\001\004\001\004\001\006\001\005\001\
\004\001\009\001\010\001\011\001\012\001\013\001\030\001\018\001\
\001\001\000\000\001\001\004\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\001\001\001\001\001\001\004\001\
\001\001\006\001\001\001\001\001\009\001\010\001\011\001\012\001\
\013\001\026\001\027\001\001\001\001\001\001\001\004\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\001\001\
\001\001\004\001\004\001\055\000\006\001\004\001\059\000\009\001\
\010\001\011\001\012\001\013\001\026\001\027\001\001\001\004\001\
\139\000\004\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\001\001\255\255\255\255\004\001\255\255\006\001\
\255\255\255\255\009\001\010\001\011\001\012\001\013\001\026\001\
\027\001\001\001\255\255\255\255\004\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\001\001\255\255\255\255\
\004\001\001\001\006\001\001\001\004\001\009\001\004\001\255\255\
\255\255\255\255\026\001\027\001\014\001\015\001\016\001\017\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\001\001\255\255\026\001\027\001\026\001\027\001\255\255\255\255\
\001\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\255\255\255\255\020\001\021\001\022\001\023\001\024\001\
\025\001\004\001\255\255\255\255\255\255\255\255\255\255\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\006\001\
\255\255\255\255\255\255\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\006\001\255\255\255\255\255\255\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\033\001\
\255\255\255\255\036\001\037\001\038\001\039\001\040\001\041\001\
\042\001\033\001\255\255\255\255\036\001\037\001\038\001\039\001\
\040\001\041\001\042\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001"

let yynames_const = "\
  SEMI\000\
  COLON\000\
  LPAREN\000\
  RPAREN\000\
  LSQUARE\000\
  RSQUARE\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  MPLUS\000\
  MMINUS\000\
  MTIMES\000\
  TIMES\000\
  DIVIDE\000\
  MDIVIDE\000\
  ASSIGN\000\
  ARROW\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  AND\000\
  OR\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  BOOLEAN\000\
  TRUE\000\
  FALSE\000\
  MATRIX\000\
  STRUCTURE\000\
  OPTION\000\
  INT\000\
  FLOAT\000\
  STRING\000\
  VOID\000\
  EOF\000\
  "

let yynames_block = "\
  INT_LIT\000\
  FLOAT_LIT\000\
  STRING_LIT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "parser.mly"
   ( [], [] )
# 374 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 36 "parser.mly"
                 ((_2 :: fst _1), snd _1 )
# 382 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 37 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 390 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'retval) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'formal_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 41 "parser.mly"
     ( { func_name =  _1.vname;
         formals = _2; 
         locals = List.rev _5;
         body = List.rev _6;
         ret =  _1.vtype
         } )
# 405 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 49 "parser.mly"
                      ( {vtype=Int; vname=_2}  )
# 412 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 50 "parser.mly"
                         ( {vtype=Float;vname= _2}  )
# 419 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 51 "parser.mly"
                        ( {vtype=Void; vname=_2 } )
# 426 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 52 "parser.mly"
                      ( {vtype=Matrix; vname=_2}  )
# 433 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 53 "parser.mly"
                       ( {vtype=Option;vname= _2}  )
# 440 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 54 "parser.mly"
                          ( {vtype=Structure;vname= _2}  )
# 447 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 55 "parser.mly"
                        ( {vtype=Boolean; vname=_2}  )
# 454 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 56 "parser.mly"
                       ( {vtype=String;vname= _2}  )
# 461 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tdecl) in
    Obj.repr(
# 63 "parser.mly"
                            ( [_1] )
# 468 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tdecl) in
    Obj.repr(
# 64 "parser.mly"
                            ( _3 :: _1 )
# 476 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
                     ( [] )
# 482 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 67 "parser.mly"
                     ( _2 :: _1 )
# 490 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tdecl) in
    Obj.repr(
# 69 "parser.mly"
                ( _1 )
# 497 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser.mly"
            ( {vname = _2; vtype = Int}  )
# 504 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 72 "parser.mly"
                   ( {vname = _2; vtype = Float}  )
# 511 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 73 "parser.mly"
                  ( {vname = _2; vtype = Void}  )
# 518 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 74 "parser.mly"
                ( {vname = _2; vtype = Matrix}  )
# 525 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "parser.mly"
                 ( {vname = _2; vtype = Option}  )
# 532 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 76 "parser.mly"
                    ( {vname = _2; vtype = Structure}  )
# 539 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "parser.mly"
                  ( {vname = _2; vtype = Boolean}  )
# 546 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 78 "parser.mly"
                 ( {vname = _2; vtype = String}  )
# 553 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
                   ( [] )
# 559 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "parser.mly"
                   ( _2 :: _1 )
# 567 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
              ( Expr(_1) )
# 574 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
                     ( Return(_2) )
# 581 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
               (Return(Noexpr))
# 587 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 88 "parser.mly"
                            ( Block(List.rev _2) )
# 594 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 89 "parser.mly"
                                              ( If(_3, _5, Block([])) )
# 602 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'b_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 90 "parser.mly"
                                              ( If(_3, _5, _7) )
# 611 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'b_expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 92 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 621 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 93 "parser.mly"
                                    ( While(_3, _5) )
# 629 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                  ( Noexpr )
# 635 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                  ( _1 )
# 642 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 100 "parser.mly"
                     ( Int_lit(_1) )
# 649 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 101 "parser.mly"
                 (Float_lit(_1))
# 656 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 102 "parser.mly"
                  (String_lit(_1))
# 663 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 103 "parser.mly"
                     ( Id(_1) )
# 670 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                     ( Binary_op(_1, Add,   _3) )
# 678 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                     ( Binary_op(_1, Sub,   _3) )
# 686 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                     ( Binary_op(_1, Times,  _3) )
# 694 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                     ( Binary_op(_1, Divide,   _3) )
# 702 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                     ( MatBinary_op(_1, MAdd,   _3) )
# 710 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                      ( MatBinary_op(_1, MSub,   _3) )
# 718 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                      ( MatBinary_op(_1, MTime,  _3) )
# 726 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                      ( MatBinary_op(_1, MDivide,   _3) )
# 734 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                     ( VarAssign(_1, _3) )
# 742 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                                                             ( ElemAssign(_1, _3, _6, _9) )
# 752 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 114 "parser.mly"
                                 ( Call(_1, _3) )
# 760 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                       ( _2 )
# 767 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                    ( Bool_expr1(_1, Eq, _3) )
# 775 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                     ( Bool_expr1(_1, Neq,   _3) )
# 783 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                     ( Bool_expr1(_1, Lt,  _3) )
# 791 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                     ( Bool_expr1(_1, Leq,   _3) )
# 799 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                     ( Bool_expr1(_1, Gt,  _3) )
# 807 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                     ( Bool_expr1(_1, Geq,   _3) )
# 815 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'b_expr) in
    Obj.repr(
# 123 "parser.mly"
                        ( Bool_expr2(_1, And,   _3) )
# 823 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'b_expr) in
    Obj.repr(
# 124 "parser.mly"
                       ( Bool_expr2(_1, Or,   _3) )
# 831 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "parser.mly"
                  ( [] )
# 837 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 129 "parser.mly"
                  ( List.rev _1 )
# 844 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                            ( [_1] )
# 851 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                            ( _3 :: _1 )
# 859 "parser.ml"
               : 'actuals_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
