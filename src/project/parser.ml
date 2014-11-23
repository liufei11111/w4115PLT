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
\004\000\004\000\004\000\004\000\005\000\005\000\007\000\007\000\
\009\000\009\000\010\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\011\000\006\000\006\000\012\000\012\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\015\000\015\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\016\000\016\000\017\000\
\017\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\006\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\001\000\003\000\001\000\002\000\
\000\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\007\000\000\000\002\000\001\000\003\000\002\000\
\003\000\002\000\003\000\005\000\007\000\009\000\005\000\002\000\
\002\000\007\000\000\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\009\000\004\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\028\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\045\000\046\000\047\000\000\000\002\000\003\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\000\015\000\040\000\
\041\000\032\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\060\000\035\000\000\000\000\000\029\000\
\033\000\000\000\000\000\000\000\000\000\000\000\000\000\011\000\
\000\000\010\000\000\000\009\000\005\000\006\000\012\000\007\000\
\000\000\000\000\000\000\000\000\025\000\016\000\024\000\023\000\
\020\000\021\000\026\000\022\000\000\000\000\000\000\000\000\000\
\000\000\000\000\055\000\051\000\052\000\056\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\059\000\000\000\000\000\028\000\014\000\000\000\069\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\067\000\000\000\
\000\000\039\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\000\037\000\
\000\000\027\000\042\000\000\000\000\000\000\000\000\000\038\000\
\000\000"

let yydgoto = "\002\000\
\003\000\072\000\023\000\024\000\053\000\029\000\054\000\025\000\
\000\000\000\000\026\000\148\000\027\000\076\000\078\000\090\000\
\091\000"

let yysindex = "\007\000\
\000\000\000\000\170\255\008\255\000\000\004\255\010\255\023\255\
\066\255\043\255\049\255\059\255\102\255\105\255\106\255\115\255\
\116\255\000\000\000\000\000\000\152\255\000\000\000\000\241\255\
\117\255\159\255\197\000\246\000\099\255\000\000\215\000\021\255\
\008\255\021\255\165\255\167\255\007\255\171\255\172\255\173\255\
\175\255\178\255\008\255\008\255\139\255\140\255\174\255\176\255\
\177\255\181\255\185\255\186\255\005\255\000\000\000\000\000\000\
\000\000\000\000\008\255\008\255\008\255\008\255\008\255\008\255\
\008\255\008\255\008\255\000\000\000\000\198\255\199\255\000\000\
\000\000\021\255\224\000\019\255\035\001\204\255\064\255\000\000\
\203\255\000\000\210\255\000\000\000\000\000\000\000\000\000\000\
\035\001\214\255\212\255\003\001\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\217\255\241\255\150\255\150\255\
\150\255\150\255\000\000\000\000\000\000\000\000\035\001\216\255\
\230\255\179\000\077\255\008\255\008\255\008\255\008\255\008\255\
\008\255\197\255\021\255\021\255\021\255\197\255\240\255\008\255\
\000\000\008\255\248\255\000\000\000\000\203\255\000\000\035\001\
\035\001\035\001\035\001\035\001\035\001\221\255\000\000\231\255\
\015\255\000\000\213\255\071\255\035\001\035\001\008\255\151\255\
\197\255\008\255\254\255\002\000\008\255\016\001\000\000\000\000\
\015\000\000\000\000\000\035\001\243\255\197\255\008\255\000\000\
\035\001"

let yyrindex = "\000\000\
\000\000\000\000\006\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\047\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\006\000\000\000\019\000\000\000\027\000\030\000\034\000\035\000\
\048\000\049\000\050\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\083\255\000\000\000\000\000\000\
\002\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\018\255\000\000\062\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\074\000\101\000\
\128\000\155\000\000\000\000\000\000\000\000\000\011\255\000\000\
\027\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\070\255\
\072\255\074\255\082\255\084\255\145\255\001\000\000\000\017\255\
\000\000\000\000\000\000\000\000\114\255\068\255\000\000\000\000\
\000\000\072\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\125\255\000\000\000\000\000\000\000\000\
\246\255"

let yygindex = "\000\000\
\000\000\003\000\000\000\000\000\000\000\176\000\231\000\236\255\
\000\000\000\000\000\000\000\000\252\255\225\255\181\000\000\000\
\000\000"

let yytablesize = 565
let yytable = "\028\000\
\036\000\031\000\079\000\055\000\030\000\022\000\004\000\001\000\
\101\000\082\000\004\000\057\000\032\000\102\000\057\000\154\000\
\057\000\068\000\057\000\057\000\068\000\072\000\122\000\074\000\
\083\000\033\000\072\000\075\000\077\000\075\000\057\000\057\000\
\057\000\057\000\057\000\057\000\057\000\057\000\089\000\092\000\
\123\000\124\000\115\000\068\000\123\000\124\000\018\000\019\000\
\020\000\021\000\018\000\019\000\020\000\021\000\103\000\104\000\
\105\000\106\000\107\000\108\000\109\000\110\000\111\000\018\000\
\019\000\020\000\021\000\126\000\034\000\114\000\061\000\073\000\
\062\000\061\000\063\000\062\000\073\000\063\000\156\000\157\000\
\135\000\055\000\064\000\044\000\065\000\064\000\044\000\065\000\
\035\000\123\000\124\000\143\000\144\000\145\000\036\000\061\000\
\061\000\062\000\062\000\063\000\063\000\004\000\123\000\124\000\
\037\000\005\000\069\000\064\000\064\000\065\000\065\000\136\000\
\137\000\138\000\139\000\140\000\141\000\056\000\075\000\075\000\
\075\000\030\000\030\000\149\000\142\000\150\000\006\000\007\000\
\146\000\008\000\009\000\045\000\031\000\031\000\070\000\071\000\
\048\000\049\000\050\000\051\000\052\000\018\000\019\000\020\000\
\021\000\066\000\158\000\038\000\066\000\077\000\039\000\040\000\
\164\000\004\000\043\000\160\000\044\000\005\000\159\000\057\000\
\041\000\042\000\169\000\063\000\064\000\065\000\066\000\080\000\
\168\000\081\000\066\000\066\000\004\000\084\000\085\000\086\000\
\005\000\087\000\006\000\007\000\088\000\008\000\009\000\045\000\
\093\000\094\000\070\000\071\000\048\000\049\000\050\000\051\000\
\052\000\018\000\019\000\020\000\021\000\006\000\007\000\004\000\
\008\000\009\000\010\000\005\000\125\000\011\000\012\000\013\000\
\014\000\015\000\016\000\017\000\018\000\019\000\020\000\021\000\
\128\000\129\000\134\000\095\000\130\000\096\000\097\000\132\000\
\006\000\007\000\098\000\008\000\009\000\045\000\099\000\100\000\
\070\000\071\000\048\000\049\000\050\000\051\000\052\000\018\000\
\019\000\020\000\021\000\112\000\113\000\127\000\058\000\083\000\
\147\000\058\000\153\000\058\000\151\000\058\000\058\000\155\000\
\123\000\162\000\163\000\036\000\167\000\074\000\043\000\036\000\
\036\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
\058\000\045\000\166\000\025\000\046\000\047\000\048\000\049\000\
\050\000\051\000\052\000\024\000\036\000\036\000\023\000\036\000\
\036\000\036\000\020\000\021\000\036\000\036\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\036\000\036\000\048\000\
\026\000\022\000\048\000\152\000\048\000\070\000\048\000\048\000\
\048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
\048\000\071\000\048\000\048\000\048\000\048\000\048\000\048\000\
\048\000\048\000\049\000\043\000\133\000\049\000\161\000\049\000\
\000\000\049\000\049\000\049\000\049\000\049\000\049\000\000\000\
\000\000\000\000\000\000\049\000\000\000\049\000\049\000\049\000\
\049\000\049\000\049\000\049\000\049\000\050\000\000\000\000\000\
\050\000\000\000\050\000\000\000\050\000\050\000\050\000\050\000\
\050\000\050\000\000\000\000\000\000\000\000\000\050\000\000\000\
\050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
\053\000\000\000\000\000\053\000\000\000\053\000\000\000\053\000\
\053\000\053\000\053\000\053\000\053\000\000\000\000\000\000\000\
\000\000\053\000\000\000\053\000\053\000\053\000\053\000\053\000\
\053\000\053\000\053\000\054\000\000\000\000\000\054\000\000\000\
\054\000\000\000\054\000\054\000\054\000\054\000\054\000\054\000\
\000\000\000\000\000\000\000\000\054\000\000\000\054\000\054\000\
\054\000\054\000\054\000\054\000\054\000\054\000\068\000\000\000\
\000\000\000\000\000\000\000\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\058\000\116\000\117\000\
\118\000\119\000\120\000\121\000\000\000\000\000\059\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\073\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\000\000\116\000\117\000\118\000\119\000\120\000\
\121\000\068\000\000\000\000\000\000\000\000\000\000\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\131\000\000\000\000\000\000\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\165\000\000\000\000\000\
\000\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\008\000\000\000\000\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000"

let yycheck = "\004\000\
\000\000\006\000\034\000\024\000\001\001\003\000\003\001\001\000\
\004\001\003\001\003\001\001\001\003\001\009\001\004\001\001\001\
\006\001\001\001\008\001\009\001\004\001\004\001\004\001\003\001\
\018\001\003\001\009\001\032\000\033\000\034\000\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\043\000\044\000\
\026\001\027\001\074\000\027\001\026\001\027\001\043\001\044\001\
\045\001\046\001\043\001\044\001\045\001\046\001\059\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\043\001\
\044\001\045\001\046\001\004\001\003\001\074\000\001\001\004\001\
\001\001\004\001\001\001\004\001\009\001\004\001\008\001\009\001\
\004\001\102\000\001\001\001\001\001\001\004\001\004\001\004\001\
\046\001\026\001\027\001\123\000\124\000\125\000\046\001\026\001\
\027\001\026\001\027\001\026\001\027\001\003\001\026\001\027\001\
\046\001\007\001\008\001\026\001\027\001\026\001\027\001\116\000\
\117\000\118\000\119\000\120\000\121\000\001\001\123\000\124\000\
\125\000\008\001\009\001\128\000\122\000\130\000\028\001\029\001\
\126\000\031\001\032\001\033\001\008\001\009\001\036\001\037\001\
\038\001\039\001\040\001\041\001\042\001\043\001\044\001\045\001\
\046\001\001\001\151\000\046\001\004\001\154\000\046\001\046\001\
\157\000\003\001\003\001\153\000\005\001\007\001\008\001\001\001\
\046\001\046\001\167\000\014\001\015\001\016\001\017\001\003\001\
\166\000\003\001\026\001\027\001\003\001\003\001\003\001\003\001\
\007\001\003\001\028\001\029\001\003\001\031\001\032\001\033\001\
\046\001\046\001\036\001\037\001\038\001\039\001\040\001\041\001\
\042\001\043\001\044\001\045\001\046\001\028\001\029\001\003\001\
\031\001\032\001\033\001\007\001\001\001\036\001\037\001\038\001\
\039\001\040\001\041\001\042\001\043\001\044\001\045\001\046\001\
\007\001\004\001\003\001\046\001\009\001\046\001\046\001\007\001\
\028\001\029\001\046\001\031\001\032\001\033\001\046\001\046\001\
\036\001\037\001\038\001\039\001\040\001\041\001\042\001\043\001\
\044\001\045\001\046\001\046\001\046\001\043\001\001\001\018\001\
\009\001\004\001\030\001\006\001\005\001\008\001\009\001\043\001\
\026\001\004\001\001\001\003\001\018\001\000\000\001\001\007\001\
\008\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\033\001\004\001\001\001\036\001\037\001\038\001\039\001\
\040\001\041\001\042\001\001\001\028\001\029\001\001\001\031\001\
\032\001\033\001\001\001\001\001\036\001\037\001\038\001\039\001\
\040\001\041\001\042\001\043\001\044\001\045\001\046\001\001\001\
\001\001\001\001\004\001\132\000\006\001\004\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\004\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\001\001\004\001\102\000\004\001\154\000\006\001\
\255\255\008\001\009\001\010\001\011\001\012\001\013\001\255\255\
\255\255\255\255\255\255\018\001\255\255\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\001\001\255\255\255\255\
\004\001\255\255\006\001\255\255\008\001\009\001\010\001\011\001\
\012\001\013\001\255\255\255\255\255\255\255\255\018\001\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\001\001\255\255\255\255\004\001\255\255\006\001\255\255\008\001\
\009\001\010\001\011\001\012\001\013\001\255\255\255\255\255\255\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\001\001\255\255\255\255\004\001\255\255\
\006\001\255\255\008\001\009\001\010\001\011\001\012\001\013\001\
\255\255\255\255\255\255\255\255\018\001\255\255\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\004\001\255\255\
\255\255\255\255\255\255\255\255\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\001\001\020\001\021\001\
\022\001\023\001\024\001\025\001\255\255\255\255\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\001\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\255\255\020\001\021\001\022\001\023\001\024\001\
\025\001\004\001\255\255\255\255\255\255\255\255\255\255\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\006\001\255\255\255\255\255\255\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\006\001\255\255\255\255\
\255\255\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\033\001\255\255\255\255\036\001\037\001\038\001\
\039\001\040\001\041\001\042\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001"

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
# 37 "parser.mly"
   ( [], [] )
# 421 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 38 "parser.mly"
                ((_2 :: fst _1), snd _1 )
# 429 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 39 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 437 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'retval) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'formal_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 43 "parser.mly"
     ( { func_name =  _1.vname;
         formals = List.rev _2; 
         body = List.rev _5;
         ret =  _1.vtype
         } )
# 450 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 50 "parser.mly"
                      ( {vtype=Int; vname=_2}  )
# 457 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 51 "parser.mly"
                         ( {vtype=Float;vname= _2}  )
# 464 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 52 "parser.mly"
                        ( {vtype=Void; vname=_2 } )
# 471 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 53 "parser.mly"
                      ( {vtype=Matrix; vname=_2}  )
# 478 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 54 "parser.mly"
                       ( {vtype=Option;vname= _2}  )
# 485 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 55 "parser.mly"
                          ( {vtype=Structure;vname= _2}  )
# 492 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 56 "parser.mly"
                        ( {vtype=Boolean; vname=_2}  )
# 499 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 57 "parser.mly"
                       ( {vtype=String;vname= _2}  )
# 506 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 64 "parser.mly"
                             ( [_1] )
# 513 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 65 "parser.mly"
                             ( _3 :: _1 )
# 521 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tdecl) in
    Obj.repr(
# 68 "parser.mly"
             ( _1 )
# 528 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "parser.mly"
              ( {vtype=Matrix; vname=_2}  )
# 535 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                     ( [] )
# 541 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 73 "parser.mly"
                     ( _2 :: _1 )
# 549 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tdecl) in
    Obj.repr(
# 76 "parser.mly"
               ( _1 )
# 556 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "parser.mly"
           ( {vname = _2; vtype = Int}  )
# 563 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
             ( {vname = _2; vtype = Float}  )
# 570 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
            ( {vname = _2; vtype = Void}  )
# 577 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 82 "parser.mly"
              ( {vname = _2; vtype = Option}  )
# 584 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "parser.mly"
                 ( {vname = _2; vtype = Structure}  )
# 591 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
               ( {vname = _2; vtype = Boolean}  )
# 598 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
              ( {vname = _2; vtype = String}  )
# 605 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 88 "parser.mly"
                                                ( {mname = _2; mtype = Matrix; mrow = _4; mcol = _6}  )
# 614 "parser.ml"
               : 'mdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
                   ( [] )
# 620 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 92 "parser.mly"
                   ( _2 :: _1 )
# 628 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
       ( [_1] )
# 635 "parser.ml"
               : 'struct_arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'struct_arg_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                              (_3 :: _1)
# 643 "parser.ml"
               : 'struct_arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
              ( Expr(_1) )
# 650 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                     ( Return(_2) )
# 657 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
               (Return(Noexpr))
# 663 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 102 "parser.mly"
                            ( Block(List.rev _2) )
# 670 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 103 "parser.mly"
                                              ( If(_3, _5, Block([])) )
# 678 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'b_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 104 "parser.mly"
                                              ( If(_3, _5, _7) )
# 687 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'b_expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 106 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 697 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 107 "parser.mly"
                                    ( While(_3, _5) )
# 705 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tdecl) in
    Obj.repr(
# 108 "parser.mly"
              ( Vardec(_1) )
# 712 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'mdecl) in
    Obj.repr(
# 109 "parser.mly"
              ( Matdec(_1) )
# 719 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'struct_arg_list) in
    Obj.repr(
# 110 "parser.mly"
                                                          (Structdec(_2, _5))
# 727 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
                  ( Noexpr )
# 733 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                  ( _1 )
# 740 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 117 "parser.mly"
                     ( Int_lit(_1) )
# 747 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 118 "parser.mly"
                 (Float_lit(_1))
# 754 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 119 "parser.mly"
                  (String_lit(_1))
# 761 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 120 "parser.mly"
                     ( Id(_1) )
# 768 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                     ( Binary_op(_1, Add,   _3) )
# 776 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                     ( Binary_op(_1, Sub,   _3) )
# 784 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                     ( Binary_op(_1, Times,  _3) )
# 792 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                     ( Binary_op(_1, Divide,   _3) )
# 800 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                     ( MatBinary_op(_1, MAdd,   _3) )
# 808 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                      ( MatBinary_op(_1, MSub,   _3) )
# 816 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                      ( MatBinary_op(_1, MTime,  _3) )
# 824 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                      ( MatBinary_op(_1, MDivide,   _3) )
# 832 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                       ( VarAssign(_1, _3) )
# 840 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                                                             ( ElemAssign(_1, _3, _6, _9) )
# 850 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 131 "parser.mly"
                                 ( Call(_1, _3) )
# 858 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                       ( Precedence_expr(_2) )
# 865 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                    ( Bool_expr1(_1, Eq, _3) )
# 873 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                     ( Bool_expr1(_1, Neq,   _3) )
# 881 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                     ( Bool_expr1(_1, Lt,  _3) )
# 889 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                     ( Bool_expr1(_1, Leq,   _3) )
# 897 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                     ( Bool_expr1(_1, Gt,  _3) )
# 905 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                     ( Bool_expr1(_1, Geq,   _3) )
# 913 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'b_expr) in
    Obj.repr(
# 141 "parser.mly"
                        ( Bool_expr2(_1, And,   _3) )
# 921 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'b_expr) in
    Obj.repr(
# 142 "parser.mly"
                       ( Bool_expr2(_1, Or,   _3) )
# 929 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'b_expr) in
    Obj.repr(
# 143 "parser.mly"
                        ( Precedence_bool_expr(_2) )
# 936 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 147 "parser.mly"
                  ( [] )
# 942 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 148 "parser.mly"
                  ( List.rev _1 )
# 949 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 151 "parser.mly"
                            ( [_1] )
# 956 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 152 "parser.mly"
                            ( _3 :: _1 )
# 964 "parser.ml"
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
