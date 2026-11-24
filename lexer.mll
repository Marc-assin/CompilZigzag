{ 
    open Parser
    open Lexing
    
    exception Lexing_error of string*int*int //ligne puis caractère

}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let ident = letter [letter digit]*
let blank = [' '  '\t' '\r']


rule token = parse 
    | blank { token lexbuf }
    | '\n' { Lexing.new_line lexbuf; token lexbuf }
    | "//" [^'\n'] { Lexing.new_line lexbuf; token lexbuf }
    | ',' { TComma }
    | '{' { TLacc }
    | '}' { TRacc }
    | '(' { TLpar}
    | ')' { TRpar}
    | ':' { TColon }
    | '=' { TEqual }
    | ';' { TSemicolon }
    | '_' { TUnder }
    | '.' { TPoint }
    | '+' { TPlus}
    | '-' { TMinus }
    | '*' { TTimes }
    | "==" { TEqq }
    | "!=" { TNeq }
    | '<' { TInf }
    | "<=" { TInfeq }
    | '>' { TSup }
    | ">=" { TSupeq }
    | '!' { TNot }
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
    | "void" { TVoid }
    | "bool" { TBool }
    | "int" { TInt }
    | "i64" { TI64 }
    | "u64" { TU64 }
    | "noreturn" { TNoreturn }
    | "std" { TStd }
    | '"' s = ([^'"' '\n'] | "\"")* '"' { TString s }
    | '@' i = ident { TBuiltin (i) }
    | s = ident { TIdent (s)}
    | eof { EOF }
    | c { raise Lexing_error (c, -1, -1)}
