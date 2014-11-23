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
\002\000\002\000\002\000\015\000\015\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\016\000\
\016\000\017\000\017\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\006\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\001\000\003\000\001\000\002\000\
\000\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\007\000\000\000\002\000\001\000\003\000\002\000\
\003\000\002\000\003\000\005\000\007\000\009\000\005\000\002\000\
\002\000\007\000\007\000\000\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\009\000\004\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\028\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\046\000\047\000\048\000\000\000\002\000\003\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\013\000\015\000\
\040\000\041\000\032\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\061\000\035\000\000\000\000\000\
\000\000\029\000\033\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\000\000\010\000\000\000\009\000\000\000\005\000\
\006\000\012\000\007\000\000\000\000\000\000\000\000\000\062\000\
\025\000\016\000\024\000\023\000\020\000\021\000\026\000\022\000\
\000\000\000\000\000\000\000\000\000\000\000\000\056\000\052\000\
\053\000\057\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\060\000\000\000\
\000\000\028\000\014\000\000\000\071\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\069\000\000\000\000\000\039\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\000\037\000\
\000\000\027\000\042\000\000\000\043\000\000\000\000\000\000\000\
\038\000\000\000"

let yydgoto = "\002\000\
\003\000\074\000\023\000\024\000\054\000\029\000\055\000\025\000\
\000\000\000\000\026\000\154\000\027\000\078\000\080\000\093\000\
\094\000"

let yysindex = "\012\000\
\000\000\000\000\174\255\009\255\000\000\005\255\002\255\021\255\
\023\255\248\254\001\255\037\255\063\255\068\255\077\255\109\255\
\113\255\000\000\000\000\000\000\012\255\000\000\000\000\229\000\
\160\255\170\255\009\000\217\000\105\255\000\000\156\000\022\255\
\009\255\022\255\173\255\175\255\097\255\098\255\177\255\179\255\
\182\255\206\255\009\255\009\255\127\255\176\255\180\255\185\255\
\189\255\190\255\208\255\211\255\212\255\018\255\000\000\000\000\
\000\000\000\000\000\000\009\255\009\255\009\255\009\255\009\255\
\009\255\009\255\009\255\009\255\000\000\000\000\213\255\215\255\
\216\255\000\000\000\000\022\255\195\000\006\255\016\001\220\255\
\080\255\000\000\209\255\000\000\218\255\000\000\221\255\000\000\
\000\000\000\000\000\000\016\001\219\255\254\255\230\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\021\000\229\000\151\255\151\255\151\255\151\255\000\000\000\000\
\000\000\000\000\016\001\028\000\017\000\018\000\179\000\148\255\
\009\255\009\255\009\255\009\255\009\255\009\255\201\255\022\255\
\022\255\022\255\201\255\040\000\009\255\009\255\000\000\009\255\
\045\000\000\000\000\000\209\255\000\000\016\001\016\001\016\001\
\016\001\016\001\016\001\022\000\000\000\050\000\008\255\000\000\
\011\000\095\255\016\001\131\255\016\001\009\255\155\255\201\255\
\009\255\062\000\076\000\009\255\078\000\243\000\000\000\000\000\
\077\000\000\000\000\000\016\001\000\000\070\000\201\255\009\255\
\000\000\016\001"

let yyrindex = "\000\000\
\000\000\000\000\089\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\047\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\089\000\000\000\090\000\000\000\092\000\102\000\103\000\105\000\
\107\000\114\000\112\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\152\255\000\000\
\000\000\000\000\239\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\065\255\000\000\113\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\069\255\074\000\101\000\128\000\000\000\000\000\
\000\000\000\000\247\255\000\000\092\000\102\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\010\255\015\255\017\255\
\084\255\101\255\223\255\001\000\000\000\019\255\000\000\000\000\
\000\000\000\000\161\255\000\000\067\255\000\000\000\000\000\000\
\116\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\181\255\000\000\000\000\000\000\000\000\
\000\000\155\000"

let yygindex = "\000\000\
\000\000\004\000\000\000\000\000\000\000\236\000\024\001\236\255\
\000\000\000\000\000\000\253\000\252\255\225\255\228\000\000\000\
\000\000"

let yytablesize = 546
let yytable = "\028\000\
\036\000\031\000\081\000\056\000\032\000\030\000\022\000\004\000\
\161\000\127\000\063\000\004\000\001\000\063\000\043\000\064\000\
\044\000\065\000\064\000\070\000\065\000\105\000\070\000\033\000\
\076\000\034\000\106\000\077\000\079\000\077\000\045\000\128\000\
\129\000\128\000\129\000\063\000\063\000\035\000\092\000\095\000\
\064\000\064\000\065\000\065\000\120\000\070\000\036\000\018\000\
\019\000\020\000\021\000\018\000\019\000\020\000\021\000\107\000\
\108\000\109\000\110\000\111\000\112\000\113\000\114\000\115\000\
\018\000\019\000\020\000\021\000\074\000\050\000\075\000\119\000\
\050\000\074\000\050\000\075\000\050\000\050\000\050\000\050\000\
\050\000\050\000\037\000\131\000\066\000\056\000\050\000\066\000\
\050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
\149\000\150\000\151\000\084\000\086\000\067\000\163\000\164\000\
\067\000\128\000\129\000\004\000\038\000\066\000\066\000\005\000\
\070\000\039\000\085\000\087\000\142\000\143\000\144\000\145\000\
\146\000\147\000\040\000\077\000\077\000\077\000\067\000\067\000\
\155\000\155\000\148\000\157\000\006\000\007\000\152\000\008\000\
\009\000\046\000\165\000\164\000\071\000\072\000\073\000\050\000\
\051\000\052\000\053\000\018\000\019\000\020\000\021\000\141\000\
\045\000\166\000\041\000\045\000\079\000\004\000\042\000\172\000\
\057\000\005\000\167\000\168\000\064\000\065\000\066\000\067\000\
\030\000\030\000\058\000\178\000\096\000\128\000\129\000\082\000\
\004\000\083\000\177\000\088\000\005\000\089\000\006\000\007\000\
\090\000\008\000\009\000\046\000\031\000\031\000\071\000\072\000\
\073\000\050\000\051\000\052\000\053\000\018\000\019\000\020\000\
\021\000\006\000\007\000\004\000\008\000\009\000\010\000\005\000\
\091\000\011\000\012\000\013\000\014\000\015\000\016\000\017\000\
\018\000\019\000\020\000\021\000\130\000\097\000\135\000\068\000\
\133\000\098\000\068\000\134\000\006\000\007\000\099\000\008\000\
\009\000\046\000\100\000\101\000\071\000\072\000\073\000\050\000\
\051\000\052\000\053\000\018\000\019\000\020\000\021\000\058\000\
\068\000\068\000\058\000\132\000\058\000\102\000\058\000\058\000\
\103\000\104\000\116\000\036\000\117\000\118\000\136\000\036\000\
\036\000\059\000\058\000\058\000\058\000\058\000\058\000\058\000\
\058\000\058\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\138\000\036\000\036\000\140\000\036\000\
\036\000\036\000\085\000\087\000\036\000\036\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\036\000\036\000\049\000\
\153\000\158\000\049\000\160\000\049\000\162\000\049\000\049\000\
\049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
\049\000\170\000\049\000\049\000\049\000\049\000\049\000\049\000\
\049\000\049\000\051\000\128\000\171\000\051\000\173\000\051\000\
\175\000\051\000\051\000\051\000\051\000\051\000\051\000\176\000\
\076\000\044\000\025\000\051\000\024\000\051\000\051\000\051\000\
\051\000\051\000\051\000\051\000\051\000\054\000\023\000\020\000\
\054\000\021\000\054\000\026\000\054\000\054\000\054\000\054\000\
\054\000\054\000\022\000\072\000\073\000\159\000\054\000\044\000\
\054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
\055\000\139\000\156\000\055\000\169\000\055\000\000\000\055\000\
\055\000\055\000\055\000\055\000\055\000\000\000\000\000\000\000\
\000\000\055\000\000\000\055\000\055\000\055\000\055\000\055\000\
\055\000\055\000\055\000\059\000\075\000\000\000\059\000\000\000\
\059\000\000\000\059\000\059\000\000\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\059\000\059\000\
\059\000\059\000\059\000\059\000\059\000\059\000\069\000\000\000\
\000\000\000\000\000\000\000\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\000\000\121\000\122\000\
\123\000\124\000\125\000\126\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\000\000\121\000\122\000\
\123\000\124\000\125\000\126\000\069\000\000\000\000\000\000\000\
\000\000\000\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\137\000\000\000\000\000\000\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\174\000\000\000\000\000\000\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\046\000\000\000\000\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\008\000\
\000\000\000\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000"

let yycheck = "\004\000\
\000\000\006\000\034\000\024\000\003\001\001\001\003\000\003\001\
\001\001\004\001\001\001\003\001\001\000\004\001\003\001\001\001\
\005\001\001\001\004\001\001\001\004\001\004\001\004\001\003\001\
\003\001\003\001\009\001\032\000\033\000\034\000\019\001\026\001\
\027\001\026\001\027\001\026\001\027\001\046\001\043\000\044\000\
\026\001\027\001\026\001\027\001\076\000\027\001\046\001\043\001\
\044\001\045\001\046\001\043\001\044\001\045\001\046\001\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\043\001\044\001\045\001\046\001\004\001\001\001\004\001\076\000\
\004\001\009\001\006\001\009\001\008\001\009\001\010\001\011\001\
\012\001\013\001\046\001\004\001\001\001\106\000\018\001\004\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\128\000\129\000\130\000\003\001\003\001\001\001\008\001\009\001\
\004\001\026\001\027\001\003\001\046\001\026\001\027\001\007\001\
\008\001\046\001\018\001\018\001\121\000\122\000\123\000\124\000\
\125\000\126\000\046\001\128\000\129\000\130\000\026\001\027\001\
\133\000\134\000\127\000\136\000\028\001\029\001\131\000\031\001\
\032\001\033\001\008\001\009\001\036\001\037\001\038\001\039\001\
\040\001\041\001\042\001\043\001\044\001\045\001\046\001\004\001\
\001\001\158\000\046\001\004\001\161\000\003\001\046\001\164\000\
\001\001\007\001\008\001\160\000\014\001\015\001\016\001\017\001\
\008\001\009\001\001\001\176\000\046\001\026\001\027\001\003\001\
\003\001\003\001\175\000\003\001\007\001\003\001\028\001\029\001\
\003\001\031\001\032\001\033\001\008\001\009\001\036\001\037\001\
\038\001\039\001\040\001\041\001\042\001\043\001\044\001\045\001\
\046\001\028\001\029\001\003\001\031\001\032\001\033\001\007\001\
\003\001\036\001\037\001\038\001\039\001\040\001\041\001\042\001\
\043\001\044\001\045\001\046\001\001\001\046\001\004\001\001\001\
\007\001\046\001\004\001\007\001\028\001\029\001\046\001\031\001\
\032\001\033\001\046\001\046\001\036\001\037\001\038\001\039\001\
\040\001\041\001\042\001\043\001\044\001\045\001\046\001\001\001\
\026\001\027\001\004\001\043\001\006\001\046\001\008\001\009\001\
\046\001\046\001\046\001\003\001\046\001\046\001\009\001\007\001\
\008\001\001\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\007\001\028\001\029\001\003\001\031\001\
\032\001\033\001\018\001\018\001\036\001\037\001\038\001\039\001\
\040\001\041\001\042\001\043\001\044\001\045\001\046\001\001\001\
\009\001\005\001\004\001\030\001\006\001\043\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\004\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\001\001\026\001\001\001\004\001\001\001\006\001\
\004\001\008\001\009\001\010\001\011\001\012\001\013\001\018\001\
\000\000\001\001\001\001\018\001\001\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\001\001\001\001\001\001\
\004\001\001\001\006\001\001\001\008\001\009\001\010\001\011\001\
\012\001\013\001\001\001\004\001\004\001\138\000\018\001\004\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\001\001\106\000\134\000\004\001\161\000\006\001\255\255\008\001\
\009\001\010\001\011\001\012\001\013\001\255\255\255\255\255\255\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\001\001\001\001\255\255\004\001\255\255\
\006\001\255\255\008\001\009\001\255\255\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\004\001\255\255\
\255\255\255\255\255\255\255\255\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\255\255\020\001\021\001\
\022\001\023\001\024\001\025\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\255\255\020\001\021\001\
\022\001\023\001\024\001\025\001\004\001\255\255\255\255\255\255\
\255\255\255\255\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\006\001\255\255\255\255\255\255\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\006\001\255\255\255\255\255\255\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\033\001\255\255\255\255\
\036\001\037\001\038\001\039\001\040\001\041\001\042\001\033\001\
\255\255\255\255\036\001\037\001\038\001\039\001\040\001\041\001\
\042\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001"

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
# 420 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 38 "parser.mly"
                ((_2 :: fst _1), snd _1 )
# 428 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 39 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 436 "parser.ml"
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
# 449 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 50 "parser.mly"
                      ( {vtype=Int; vname=_2}  )
# 456 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 51 "parser.mly"
                         ( {vtype=Float;vname= _2}  )
# 463 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 52 "parser.mly"
                        ( {vtype=Void; vname=_2 } )
# 470 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 53 "parser.mly"
                      ( {vtype=Matrix; vname=_2}  )
# 477 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 54 "parser.mly"
                       ( {vtype=Option;vname= _2}  )
# 484 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 55 "parser.mly"
                          ( {vtype=Structure;vname= _2}  )
# 491 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 56 "parser.mly"
                        ( {vtype=Boolean; vname=_2}  )
# 498 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 57 "parser.mly"
                       ( {vtype=String;vname= _2}  )
# 505 "parser.ml"
               : 'retval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 64 "parser.mly"
                             ( [_1] )
# 512 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 65 "parser.mly"
                             ( _3 :: _1 )
# 520 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tdecl) in
    Obj.repr(
# 68 "parser.mly"
             ( _1 )
# 527 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "parser.mly"
              ( {vtype=Matrix; vname=_2}  )
# 534 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                     ( [] )
# 540 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 73 "parser.mly"
                     ( _2 :: _1 )
# 548 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tdecl) in
    Obj.repr(
# 76 "parser.mly"
               ( _1 )
# 555 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "parser.mly"
           ( {vname = _2; vtype = Int}  )
# 562 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
             ( {vname = _2; vtype = Float}  )
# 569 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
            ( {vname = _2; vtype = Void}  )
# 576 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 82 "parser.mly"
              ( {vname = _2; vtype = Option}  )
# 583 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "parser.mly"
                 ( {vname = _2; vtype = Structure}  )
# 590 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
               ( {vname = _2; vtype = Boolean}  )
# 597 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
              ( {vname = _2; vtype = String}  )
# 604 "parser.ml"
               : 'tdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 88 "parser.mly"
                                                ( {mname = _2; mtype = Matrix; mrow = _4; mcol = _6}  )
# 613 "parser.ml"
               : 'mdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
                   ( [] )
# 619 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 92 "parser.mly"
                   ( _2 :: _1 )
# 627 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
       ( [_1] )
# 634 "parser.ml"
               : 'struct_arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'struct_arg_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                              (_3 :: _1)
# 642 "parser.ml"
               : 'struct_arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
              ( Expr(_1) )
# 649 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                     ( Return(_2) )
# 656 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
               (Return(Noexpr))
# 662 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 102 "parser.mly"
                            ( Block(List.rev _2) )
# 669 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 103 "parser.mly"
                                              ( If(_3, _5, Block([])) )
# 677 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'b_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 104 "parser.mly"
                                              ( If(_3, _5, _7) )
# 686 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'b_expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 106 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 696 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 107 "parser.mly"
                                    ( While(_3, _5) )
# 704 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tdecl) in
    Obj.repr(
# 108 "parser.mly"
              ( Vardec(_1) )
# 711 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'mdecl) in
    Obj.repr(
# 109 "parser.mly"
              ( Matdec(_1) )
# 718 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'struct_arg_list) in
    Obj.repr(
# 110 "parser.mly"
                                                          (Structdec(_2, _5))
# 726 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'struct_arg_list) in
    Obj.repr(
# 111 "parser.mly"
                                                       (Optiondec(_2, _5))
# 734 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
                  ( Noexpr )
# 740 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                  ( _1 )
# 747 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 118 "parser.mly"
                     ( Int_lit(_1) )
# 754 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 119 "parser.mly"
                 (Float_lit(_1))
# 761 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 120 "parser.mly"
                  (String_lit(_1))
# 768 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 121 "parser.mly"
                     ( Id(_1) )
# 775 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                     ( Binary_op(_1, Add,   _3) )
# 783 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                     ( Binary_op(_1, Sub,   _3) )
# 791 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                     ( Binary_op(_1, Times,  _3) )
# 799 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                     ( Binary_op(_1, Divide,   _3) )
# 807 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                     ( MatBinary_op(_1, MAdd,   _3) )
# 815 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                      ( MatBinary_op(_1, MSub,   _3) )
# 823 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                      ( MatBinary_op(_1, MTime,  _3) )
# 831 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                      ( MatBinary_op(_1, MDivide,   _3) )
# 839 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                       ( VarAssign(_1, _3) )
# 847 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                                                             ( ElemAssign(_1, _3, _6, _9) )
# 857 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 132 "parser.mly"
                                 ( Call(_1, _3) )
# 865 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                       ( Precedence_expr(_2) )
# 872 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 134 "parser.mly"
               (Struct_element(_1, _3))
# 880 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                    ( Bool_expr1(_1, Eq, _3) )
# 888 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                     ( Bool_expr1(_1, Neq,   _3) )
# 896 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
                     ( Bool_expr1(_1, Lt,  _3) )
# 904 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
                     ( Bool_expr1(_1, Leq,   _3) )
# 912 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                     ( Bool_expr1(_1, Gt,  _3) )
# 920 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                     ( Bool_expr1(_1, Geq,   _3) )
# 928 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'b_expr) in
    Obj.repr(
# 143 "parser.mly"
                        ( Bool_expr2(_1, And,   _3) )
# 936 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'b_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'b_expr) in
    Obj.repr(
# 144 "parser.mly"
                       ( Bool_expr2(_1, Or,   _3) )
# 944 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'b_expr) in
    Obj.repr(
# 145 "parser.mly"
                        ( Precedence_bool_expr(_2) )
# 951 "parser.ml"
               : 'b_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 149 "parser.mly"
                  ( [] )
# 957 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 150 "parser.mly"
                  ( List.rev _1 )
# 964 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 153 "parser.mly"
                            ( [_1] )
# 971 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 154 "parser.mly"
                            ( _3 :: _1 )
# 979 "parser.ml"
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
