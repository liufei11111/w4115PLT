{ 
	open Lexing
	open Parser 
}
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = ['-']? digit* frac? exp?

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "{*"     { comment lexbuf } (* Comments *)
| '('      { LPAREN } (* precedence, matrix initialization, ifelseforwhile *)
| ')'      { RPAREN }
| '{'      { LBRACE } (* block of statements *)
| '}'      { RBRACE }
| '['      { LSQUARE } (* matrix index access *)
| ']'      { RSQUARE}
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| "+."		 { MPLUS } (* plus matrix and matirx *)
| "+.."    { MIPLUS } (* plus matirx and int/float *)
| '-'      { MINUS }
| "-."		 { MMINUS }
| "-.."    { MIMINUS }
| '*'      { TIMES }
| "*."     { MTIMES }
| "*.."    { MITIMES }
| '/'      { DIVIDE }
| "/."     { MDIVIDE }
| "/.."    { MIDIVIDE }
| '='      { ASSIGN }
| "&&"     { AND }
| "||"     { OR }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "->"     { ARROW } (* access struct/option element *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "Boolean"{ BOOLEAN }
| "true"   { TRUE }
| "false"  { FALSE}
| "Matrix" { MATRIX } (* variable type Matrix *)
| '''      { TRANSPOSE } (* option for Matrix *)
| '~'      { INVERSION }
| '^'      { DETERMINANT } 
| "Structure" { STRUCTURE } (* variable type Structure *)
| "Option" { OPTION } (* variable type Option *)
| "Int"    { INT }
| "Float"  { FLOAT }
| "String" { STRING }
| "Void"   { VOID }
| ['-']?['0'-'9']+ as lxm { INT_LIT(int_of_string lxm) } (* integer literal *)
| float    { FLOAT_LIT (float_of_string (Lexing.lexeme lexbuf)) } (* float literal *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) } (* id/function name *)
| '"' (([' '-'!' '#'-'[' ']'-'~'] | '\\' ['\\' '"' 'n' 'r' 't'])* as s) '"' 
                       { STRING_LIT(s) } (* string literal *)
| eof { EOF }
| _  { raise (Failure("illegal character " )) }

and comment = parse
  "*}" { token lexbuf }
| _    { comment lexbuf }