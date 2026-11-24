{exception Parsing_error string*int*int}

%token TBreak
%token TContinue
%token TAnd
%token TConst
%token TDefer
%token TElse
%token TFn
%token TIf
%token TOr
%token TPub
%token TReturn
%token TUnreachable
%token TVar
%token TWhile
%token TComma
%token TRpar
%token TLpar
%token TRacc
%token TLacc
%token TColon
%token TEqual
%token TSemicolon
%token TUnder
%token TPoint
%token TPlus
%token TMinus 
%token TTimes
%token TEqq 
%token TNeq
%token TInf 
%token TInfeq 
%token TSup 
%token TSupeq 
%token TNot
%token <string> TBuiltin
%token <string> TIdent
%token <string> TString
%token <int> TInt

%left TOr
%left TAnd 
%nonassoc TEqq TNeq TSup TSupeq TInf TInfeq
%left TPlus TMinus 
%nonassoc TNeg TNot

%type <Ast.program> file
%%

file : std f = fundef* EOF {AProgram(AStd, f)}

std : TConst TStd TEqual b = TBuiltin TLpar s = TString TRpar TSemicolon
        { if b = "import" && s = "std" then () else raise Parsing_error ("Oublié d'importer std", -1, -1)}

fundef : pub = TPub? TFn s = TIdent TLpar p = separated_list(TComma, param) TRpar t = type b = block 
        { AFundef ( match pub with 
                    | Some _ -> true 
                    | None -> false, s, p, t, b)}

param :  (TUnder | s = TIdent)  ':' t = type 
        { AParam (option(s), t)}

type : s = ident 
        { AIdent s }

block : TLacc sl = stmt* TRacc
        { ABlock sl }

stmt : e = expr TSemicolon
        { e }
    | TReturn e = expr? TSemicolon
        { AReturn e}
    | t1 = (TVar | TConst) s = TIdent TColon t2 = type TEqual e = expr TSemicolon
        { ADef (t1, s, t, e) }
    | TIf TLpar ex = expr TRpar b = block e = else?
        { AIf (ex, b, e) }
    | TWhile TLpar e = expr TRpar b = block
        { AWhile (e, b)}
    | TBreak TSemicolon
        { ABreak }
    | TContinue TSemicolon
        { AContinue }
    | TDefer s = simple-stmt 
        { ADefer s}
    | s = simple-stmt
        { s }

simple-stmt : b = block
        { b }
    | ( TUnder | i = ident ) TEqual e = expr
        { ASplstmt (i, e) }
    | TStd TPoint TDebug TPoint TPrint TLpar s = TString TComma TPoint TLacc p = separated_list(TComma, expr) TRacc TRpar TSemicolon
        { APrint (s, p) }

else : TElse b = block 
        { b }
    | TElse TIf TLpar ex = expr TRpar b = block el = else?
        { AIf (ex, b, el)}

expr : 
    | i = Tint
        { AInt i }
    | i = TIdent
        { AIdent i }
    | TUnreachable
        { AUnreachable }
    | TNot e = expr 
        { ANot e}
    | TMinus e = expr %prec TNeg
        { AMinus e }
    | e1 = expr b = binop e2 = expr 
        { ABinop (b, e1, e2) }
    | f = (TIdent | TBuiltin) TLpar e = separated_list(TComma, expr) TRpar
        { AFun (f, e)}
    | TLpar e = expr TRpar 
        { e }

binop :
    TEqq
        { AEqq }
    | TNeq
        { ANeq }
    | TInf
        { AInf }
    | TInfeq
        { AInfeq }
    | TSup
        { ASup }
    | TSupeq
        { ASupeq }
    | TPlus
        { APlus }
    | TMinus 
        { AMinus }
    | TTimes
        { ATimes }
    | TAnd 
        { AAnd }
    | TOr 
        { AOr }

