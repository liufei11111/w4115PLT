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
\008\000\008\000\011\000\006\000\006\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\014\000\
\014\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\015\000\015\000\016\000\016\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\006\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\001\000\003\000\001\000\002\000\
\000\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\007\000\000\000\002\000\002\000\003\000\002\000\
\003\000\005\000\007\000\009\000\005\000\002\000\002\000\000\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\009\000\004\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\028\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\042\000\043\000\044\000\000\000\002\000\003\000\000\000\
\000\000\000\000\000\000\000\000\000\000\032\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\000\015\000\038\000\
\039\000\030\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\057\000\033\000\000\000\029\000\031\000\
\000\000\000\000\000\000\000\000\000\000\000\000\011\000\000\000\
\010\000\009\000\005\000\006\000\012\000\007\000\000\000\000\000\
\000\000\000\000\025\000\016\000\024\000\023\000\020\000\021\000\
\026\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
\052\000\048\000\049\000\053\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\056\000\000\000\000\000\028\000\
\014\000\000\000\066\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\064\000\000\000\000\000\037\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\000\035\000\
\000\000\027\000\000\000\000\000\000\000\036\000\000\000"

let yydgoto = "\002\000\
\003\000\071\000\023\000\024\000\053\000\029\000\054\000\025\000\
\000\000\000\000\026\000\027\000\075\000\077\000\088\000\089\000"

let yysindex = "\014\000\
\000\000\000\000\186\255\021\255\000\000\005\255\016\255\022\255\
\023\255\248\254\251\254\001\255\031\255\061\255\062\255\073\255\
\081\255\000\000\000\000\000\000\089\255\000\000\000\000\225\000\
\053\255\067\255\170\255\213\000\123\255\000\000\182\000\028\255\
\021\255\028\255\121\255\133\255\136\255\137\255\139\255\140\255\
\142\255\144\255\021\255\021\255\130\255\131\255\132\255\145\255\
\148\255\151\255\155\255\156\255\013\255\000\000\000\000\000\000\
\000\000\000\000\021\255\021\255\021\255\021\255\021\255\021\255\
\021\255\021\255\021\255\000\000\000\000\175\255\000\000\000\000\
\028\255\191\000\078\255\012\001\147\255\111\255\000\000\114\255\
\000\000\000\000\000\000\000\000\000\000\000\000\012\001\188\255\
\181\255\226\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\172\255\225\000\118\255\118\255\118\255\118\255\
\000\000\000\000\000\000\000\000\012\001\230\255\003\000\146\255\
\021\255\021\255\021\255\021\255\021\255\021\255\213\255\028\255\
\028\255\028\255\213\255\226\255\000\000\021\255\229\255\000\000\
\000\000\114\255\000\000\012\001\012\001\012\001\012\001\012\001\
\012\001\206\255\000\000\211\255\026\255\000\000\195\255\012\001\
\021\255\167\255\213\255\021\255\235\255\239\000\000\000\000\000\
\239\255\000\000\222\255\213\255\021\255\000\000\012\001"

let yyrindex = "\000\000\
\000\000\000\000\247\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\047\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\247\255\000\000\004\000\000\000\005\000\009\000\010\000\011\000\
\021\000\030\000\031\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\255\000\000\000\000\000\000\235\000\
\000\000\000\000\000\000\000\000\000\000\000\000\066\255\000\000\
\032\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\075\255\074\000\101\000\128\000\
\000\000\000\000\000\000\000\000\155\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\255\008\255\010\255\017\255\019\255\
\102\255\001\000\000\000\119\255\000\000\000\000\000\000\074\255\
\000\000\000\000\000\000\045\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\164\000"

let yygindex = "\000\000\
\000\000\002\000\000\000\000\000\000\000\178\000\208\000\236\255\
\000\000\000\000\000\000\252\255\225\255\162\000\000\000\000\000"

let yytablesize = 542
let yytable = "\028\000\
\034\000\031\000\078\000\055\000\022\000\030\000\058\000\004\000\
\059\000\058\000\060\000\059\000\041\000\060\000\001\000\041\000\
\099\000\061\000\032\000\062\000\061\000\100\000\062\000\004\000\
\033\000\034\000\148\000\074\000\076\000\074\000\073\000\058\000\
\058\000\059\000\059\000\060\000\060\000\035\000\087\000\090\000\
\036\000\112\000\061\000\061\000\062\000\062\000\037\000\018\000\
\019\000\020\000\021\000\120\000\121\000\056\000\101\000\102\000\
\103\000\104\000\105\000\106\000\107\000\108\000\109\000\018\000\
\019\000\020\000\021\000\057\000\111\000\069\000\018\000\019\000\
\020\000\021\000\069\000\046\000\038\000\070\000\046\000\055\000\
\046\000\119\000\070\000\046\000\046\000\046\000\046\000\046\000\
\139\000\140\000\141\000\043\000\046\000\044\000\046\000\046\000\
\046\000\046\000\046\000\046\000\046\000\046\000\063\000\120\000\
\121\000\063\000\039\000\040\000\132\000\133\000\134\000\135\000\
\136\000\137\000\123\000\074\000\074\000\074\000\041\000\065\000\
\138\000\144\000\065\000\079\000\142\000\004\000\042\000\063\000\
\063\000\005\000\069\000\063\000\064\000\065\000\066\000\080\000\
\120\000\121\000\081\000\082\000\150\000\083\000\084\000\076\000\
\085\000\065\000\086\000\122\000\152\000\131\000\006\000\007\000\
\159\000\008\000\009\000\045\000\124\000\158\000\070\000\047\000\
\048\000\049\000\050\000\051\000\052\000\018\000\019\000\020\000\
\021\000\004\000\058\000\120\000\121\000\005\000\151\000\091\000\
\092\000\093\000\128\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\004\000\126\000\094\000\125\000\
\005\000\095\000\006\000\007\000\096\000\008\000\009\000\045\000\
\097\000\098\000\070\000\047\000\048\000\049\000\050\000\051\000\
\052\000\018\000\019\000\020\000\021\000\006\000\007\000\004\000\
\008\000\009\000\010\000\005\000\110\000\011\000\012\000\013\000\
\014\000\015\000\016\000\017\000\018\000\019\000\020\000\021\000\
\130\000\145\000\143\000\147\000\120\000\149\000\154\000\157\000\
\006\000\007\000\156\000\008\000\009\000\045\000\071\000\040\000\
\070\000\047\000\048\000\049\000\050\000\051\000\052\000\018\000\
\019\000\020\000\021\000\034\000\025\000\024\000\068\000\034\000\
\034\000\023\000\020\000\021\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\026\000\113\000\114\000\
\115\000\116\000\117\000\118\000\034\000\034\000\022\000\034\000\
\034\000\034\000\067\000\068\000\034\000\034\000\034\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\034\000\045\000\
\040\000\146\000\045\000\129\000\045\000\153\000\000\000\045\000\
\045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
\045\000\000\000\045\000\045\000\045\000\045\000\045\000\045\000\
\045\000\045\000\047\000\000\000\000\000\047\000\000\000\047\000\
\000\000\000\000\047\000\047\000\047\000\047\000\047\000\000\000\
\000\000\000\000\000\000\047\000\000\000\047\000\047\000\047\000\
\047\000\047\000\047\000\047\000\047\000\050\000\000\000\000\000\
\050\000\000\000\050\000\000\000\000\000\050\000\050\000\050\000\
\050\000\050\000\000\000\000\000\000\000\000\000\050\000\000\000\
\050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
\051\000\000\000\000\000\051\000\000\000\051\000\000\000\000\000\
\051\000\051\000\051\000\051\000\051\000\000\000\000\000\000\000\
\000\000\051\000\000\000\051\000\051\000\051\000\051\000\051\000\
\051\000\051\000\051\000\054\000\000\000\000\000\054\000\000\000\
\054\000\000\000\000\000\054\000\055\000\000\000\000\000\055\000\
\000\000\055\000\000\000\000\000\055\000\000\000\054\000\054\000\
\054\000\054\000\054\000\054\000\054\000\054\000\072\000\055\000\
\055\000\055\000\055\000\055\000\055\000\055\000\055\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\000\000\113\000\114\000\115\000\116\000\117\000\118\000\
\068\000\000\000\000\000\000\000\000\000\000\000\059\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\127\000\
\000\000\000\000\000\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\155\000\000\000\000\000\000\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\045\000\000\000\000\000\046\000\047\000\048\000\049\000\
\050\000\051\000\052\000\008\000\000\000\000\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000"

let yycheck = "\004\000\
\000\000\006\000\034\000\024\000\003\000\001\001\001\001\003\001\
\001\001\004\001\001\001\004\001\001\001\004\001\001\000\004\001\
\004\001\001\001\003\001\001\001\004\001\009\001\004\001\003\001\
\003\001\003\001\001\001\032\000\033\000\034\000\003\001\026\001\
\027\001\026\001\027\001\026\001\027\001\046\001\043\000\044\000\
\046\001\073\000\026\001\027\001\026\001\027\001\046\001\043\001\
\044\001\045\001\046\001\026\001\027\001\001\001\059\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\043\001\
\044\001\045\001\046\001\001\001\073\000\004\001\043\001\044\001\
\045\001\046\001\009\001\001\001\046\001\004\001\004\001\100\000\
\006\001\004\001\009\001\009\001\010\001\011\001\012\001\013\001\
\120\000\121\000\122\000\003\001\018\001\005\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\001\001\026\001\
\027\001\004\001\046\001\046\001\113\000\114\000\115\000\116\000\
\117\000\118\000\004\001\120\000\121\000\122\000\046\001\001\001\
\119\000\126\000\004\001\003\001\123\000\003\001\046\001\026\001\
\027\001\007\001\008\001\014\001\015\001\016\001\017\001\003\001\
\026\001\027\001\003\001\003\001\145\000\003\001\003\001\148\000\
\003\001\027\001\003\001\001\001\147\000\004\001\028\001\029\001\
\157\000\031\001\032\001\033\001\043\001\156\000\036\001\037\001\
\038\001\039\001\040\001\041\001\042\001\043\001\044\001\045\001\
\046\001\003\001\001\001\026\001\027\001\007\001\008\001\046\001\
\046\001\046\001\007\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\003\001\009\001\046\001\004\001\
\007\001\046\001\028\001\029\001\046\001\031\001\032\001\033\001\
\046\001\046\001\036\001\037\001\038\001\039\001\040\001\041\001\
\042\001\043\001\044\001\045\001\046\001\028\001\029\001\003\001\
\031\001\032\001\033\001\007\001\046\001\036\001\037\001\038\001\
\039\001\040\001\041\001\042\001\043\001\044\001\045\001\046\001\
\003\001\005\001\009\001\030\001\026\001\043\001\004\001\018\001\
\028\001\029\001\004\001\031\001\032\001\033\001\000\000\001\001\
\036\001\037\001\038\001\039\001\040\001\041\001\042\001\043\001\
\044\001\045\001\046\001\003\001\001\001\001\001\004\001\007\001\
\008\001\001\001\001\001\001\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\001\001\020\001\021\001\
\022\001\023\001\024\001\025\001\028\001\029\001\001\001\031\001\
\032\001\033\001\004\001\004\001\036\001\037\001\038\001\039\001\
\040\001\041\001\042\001\043\001\044\001\045\001\046\001\001\001\
\004\001\128\000\004\001\100\000\006\001\148\000\255\255\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\255\255\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\001\001\255\255\255\255\004\001\255\255\006\001\
\255\255\255\255\009\001\010\001\011\001\012\001\013\001\255\255\
\255\255\255\255\255\255\018\001\255\255\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\001\001\255\255\255\255\
\004\001\255\255\006\001\255\255\255\255\009\001\010\001\011\001\
\012\001\013\001\255\255\255\255\255\255\255\255\018\001\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\001\001\255\255\255\255\004\001\255\255\006\001\255\255\255\255\
\009\001\010\001\011\001\012\001\013\001\255\255\255\255\255\255\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\001\001\255\255\255\255\004\001\255\255\
\006\001\255\255\255\255\009\001\001\001\255\255\255\255\004\001\
\255\255\006\001\255\255\255\255\009\001\255\255\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\001\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\255\255\020\001\021\001\022\001\023\001\024\001\025\001\
\004\001\255\255\255\255\255\255\255\255\255\255\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\006\001\
\255\255\255\255\255\255\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\006\001\255\255\255\255\255\255\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\033\001\255\255\255\255\036\001\037\001\038\001\039\001\
\040\001\041\001\042\001\033\001\255\255\255\255\036\001\037\001\
\038\001\039\001\040\001\041\001\042\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001"

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
# 405 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 38 "parser.mly"
                ((_2 :: fst _1), snd _1 )
# 413 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 39 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 421 "parser.ml"
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
# 434 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 50 "parser.mly"
                      ( {vtype=Int; vname=_2}  )
# 441 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 51 "parser.mly"
                         ( {vtype=Float;vname= _2}  )
# 448 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 52 "parser.mly"
                        ( {vtype=Void; vname=_2 } )
# 455 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 53 "parser.mly"
                      ( {vtype=Matrix; vname=_2}  )
# 462 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 54 "parser.mly"
                       ( {vtype=Option;vname= _2}  )
# 469 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 55 "parser.mly"
                          ( {vtype=Structure;vname= _2}  )
# 476 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 56 "parser.mly"
                        ( {vtype=Boolean; vname=_2}  )
# 483 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 57 "parser.mly"
                       ( {vtype=String;vname= _2}  )
# 490 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 64 "parser.mly"
                             ( [_1] )
# 497 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 65 "parser.mly"
                             ( _3 :: _1 )
# 505 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tdecl) in
    Obj.repr(
# 68 "parser.mly"
             ( _1 )
# 512 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "parser.mly"
              ( {vtype=Matrix; vname=_2}  )
# 519 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                     ( [] )
# 525 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 73 "parser.mly"
                     ( _2 :: _1 )
# 533 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tdecl) in
    Obj.repr(
# 76 "parser.mly"
               ( _1 )
# 540 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "parser.mly"
           ( {vname = _2; vtype = Int}  )
# 547 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
             ( {vname = _2; vtype = Float}  )
# 554 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
            ( {vname = _2; vtype = Void}  )
# 561 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 82 "parser.mly"
              ( {vname = _2; vtype = Option}  )
# 568 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "parser.mly"
                 ( {vname = _2; vtype = Structure}  )
# 575 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
               ( {vname = _2; vtype = Boolean}  )
# 582 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
              ( {vname = _2; vtype = String}  )
# 589 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 88 "parser.mly"
                                                ( {mname = _2; mtype = Matrix; mrow = _4; mcol = _6}  )
# 598 "parser.ml"
               : 'mdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
                   ( [] )
# 604 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 92 "parser.mly"
                   ( _2 :: _1 )
# 612 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
              ( Expr(_1) )
# 619 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                     ( Return(_2) )
# 626 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
               (Return(Noexpr))
# 632 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 98 "parser.mly"
                            ( Block(List.rev _2) )
# 639 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 99 "parser.mly"
                                              ( If(_3, _5, Block([])) )
# 647 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'b_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 100 "parser.mly"
                                              ( If(_3, _5, _7) )
# 656 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'b_expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 102 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 666 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 103 "parser.mly"
                                    ( While(_3, _5) )
# 674 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tdecl) in
    Obj.repr(
# 104 "parser.mly"
              ( Vardec(_1) )
# 681 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'mdecl) in
    Obj.repr(
# 105 "parser.mly"
              ( Matdec(_1) )
# 688 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
                  ( Noexpr )
# 694 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                  ( _1 )
# 701 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 112 "parser.mly"
                     ( Int_lit(_1) )
# 708 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 113 "parser.mly"
                 (Float_lit(_1))
# 715 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 114 "parser.mly"
                  (String_lit(_1))
# 722 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 115 "parser.mly"
                     ( Id(_1) )
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                     ( Binary_op(_1, Add,   _3) )
# 737 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                     ( Binary_op(_1, Sub,   _3) )
# 745 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                     ( Binary_op(_1, Times,  _3) )
# 753 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                     ( Binary_op(_1, Divide,   _3) )
# 761 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                     ( MatBinary_op(_1, MAdd,   _3) )
# 769 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                      ( MatBinary_op(_1, MSub,   _3) )
# 777 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                      ( MatBinary_op(_1, MTime,  _3) )
# 785 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                      ( MatBinary_op(_1, MDivide,   _3) )
# 793 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                       ( VarAssign(_1, _3) )
# 801 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                                                             ( ElemAssign(_1, _3, _6, _9) )
# 811 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 126 "parser.mly"
                                 ( Call(_1, _3) )
# 819 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                       ( Precedence_expr(_2) )
# 826 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                    ( Bool_expr1(_1, Eq, _3) )
# 834 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                     ( Bool_expr1(_1, Neq,   _3) )
# 842 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                     ( Bool_expr1(_1, Lt,  _3) )
# 850 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                     ( Bool_expr1(_1, Leq,   _3) )
# 858 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                     ( Bool_expr1(_1, Gt,  _3) )
# 866 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                     ( Bool_expr1(_1, Geq,   _3) )
# 874 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'b_expr) in
    Obj.repr(
# 136 "parser.mly"
                        ( Bool_expr2(_1, And,   _3) )
# 882 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'b_expr) in
    Obj.repr(
# 137 "parser.mly"
                       ( Bool_expr2(_1, Or,   _3) )
# 890 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'b_expr) in
    Obj.repr(
# 138 "parser.mly"
                        ( Precedence_bool_expr(_2) )
# 897 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "parser.mly"
                  ( [] )
# 903 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 143 "parser.mly"
                  ( List.rev _1 )
# 910 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
                            ( [_1] )
# 917 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 147 "parser.mly"
                            ( _3 :: _1 )
# 925 "parser.ml"
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
