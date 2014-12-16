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
| "{*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }(*associative, initialization, ifelseforwhile*)
| ')'      { RPAREN }
| '{'      { LBRACE }(*block of statements*)
| '}'      { RBRACE }
| '['      { LSQUARE }(*index access*)
| ']'      { RSQUARE}
| ';'      { SEMI }
| ':'      { COLON }(*key:map*)
| ','      { COMMA }
| '+'      { PLUS }
| "+."		 {MPLUS}
| "+.."    {MIPLUS}
| '-'      { MINUS }
| "-."			{MMINUS}
| "-.."    {MIMINUS}
| '*'      { TIMES }
| "*."      { MTIMES }
| "*.."    {MITIMES}
| '/'      { DIVIDE }
| "/."      { MDIVIDE }
| "/.."      { MIDIVIDE }
| '='      { ASSIGN }
| "&&"     { AND }
| "||"     { OR }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "->"      { ARROW }(*access struct element*)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "Boolean" { BOOLEAN }
| "true"  { TRUE }
| "false"  { FALSE}
| "Matrix" { MATRIX }
| '''      { TRANSPOSE }
| '~'      { INVERSION }
| '^'      { DETERMINANT } 
| "Structure" { STRUCTURE }
| "Option" { OPTION }
| "Int" {INT}
| "Float" {FLOAT}
| "String" {STRING}
| "Void" {VOID}
(*| "to" { TO }*)
| ['-']?['0'-'9']+ as lxm { INT_LIT(int_of_string lxm) }(*integer*)
| float    { FLOAT_LIT (float_of_string (Lexing.lexeme lexbuf)) }(*float*)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }(*var name*)
| '"' (([' '-'!' '#'-'[' ']'-'~'] | '\\' ['\\' '"' 'n' 'r' 't'])* as s) '"' 
                       { STRING_LIT(s) }(*string literal*)
| eof { EOF }
| _  { raise (Failure("illegal character " )) }

and comment = parse
  "*}" { token lexbuf }
| _    { comment lexbuf }