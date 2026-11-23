{ 
    open Parser
    open Lexing
    
    exception Lexing_error of string*int*int //ligne puis caractère

}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let ident = letter [letter digit]*
let builtin = '@' ident 
let blank = [' '  '\t' '\r']

rule token = parse 
    | blank { token lexbuf }
    | '\n' { Lexing.new_line lexbuf; token lexbuf }
    | "//" [^'\n'] { Lexing.new_line lexbuf; token lexbuf }
    | "break" { TBreak }
    | "continue" { TContinue }
    | "and" { TAnd }
    | "const" { TConst }
    | "defer" { TDefer }
    | "else" { TElse }
    | "fn" { TFn }
    | "if" { TIf }
    | "or" { TOr }
    | "pub" { TPub }
    | "return" { TReturn }
    | "unreachable" { TUnreachable }
    | "var" { TVar }
    | "while" { TWhile }
    | c { raise Lexing_error (c, -1, -1)}
