(* calc.yacc *)
(* User  declarations *)
%%
(* required declarations *)
%name Calc

%term
  VAR of string | NUM of int 
| PLUS | TIMES | MINUS | XOR | OR | AND | NEGATE | IMPLIES | NOT | EQUALS | LESSTHAN | GREATERTHAN | RPAREN | LPAREN | CONST of string | EOF 
| EQ | LET | IN | END | VAL | IF | THEN | ELSE | FI | TERM | ARROW | COLON | FN | FUN | DARROW | INT of string | BOOL of string

(* note the change below *)
%nonterm EXP of AST.exp | START of AST.exp  | DECL of AST.decl  | PRGM of AST.exp | STMT of AST.exp | TYPE of AST.typ 

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)
%right DARROW
%right ARROW


%right EQ
%left EQUALS 
%right IF THEN ELSE FI
%right IMPLIES
%left OR 
%left AND 
%left XOR
%right NOT
%left GREATERTHAN LESSTHAN 
%left PLUS MINUS 
%left TIMES 
%right NEGATE

%start START 
%verbose 

%%
START: PRGM (PRGM)

PRGM: STMT (STMT)

STMT : EXP TERM PRGM (AST.StmtExp(EXP))
      | EXP TERM (AST.StmtExp(EXP))

DECL : VAL VAR EQ EXP (AST.ValDecl(VAR, EXP))




EXP: 
      LPAREN EXP RPAREN (AST.BracketExp(EXP))
    | IF EXP THEN EXP ELSE EXP FI (AST.ItefExp(EXP1, EXP2, EXP3))
    | EXP IMPLIES EXP (AST.BinExp(AST.Implies, EXP1, EXP2))
    | EXP EQUALS EXP (AST.BinExp(AST.Equals, EXP1, EXP2))
    | EXP XOR EXP (AST.BinExp(AST.Xor, EXP1, EXP2))
    | EXP OR EXP  (AST.BinExp(AST.Or, EXP1, EXP2))
    | EXP AND EXP (AST.BinExp(AST.And, EXP1, EXP2))
    | EXP PLUS EXP (AST.BinExp(AST.Plus, EXP1, EXP2))
    | EXP MINUS EXP (AST.BinExp(AST.Minus, EXP1, EXP2))
    | EXP TIMES EXP (AST.BinExp(AST.Times, EXP1, EXP2))
    | LET DECL IN EXP END (AST.LetExp(DECL, EXP))
    | EXP LESSTHAN EXP (AST.BinExp(AST.Lessthan, EXP1, EXP2))
    | EXP GREATERTHAN EXP (AST.BinExp(AST.Greaterthan, EXP1, EXP2))
    | NOT EXP (AST.UnExp(AST.Not, EXP))
    | NEGATE EXP (AST.UnExp(AST.Negate, EXP))
    | CONST (AST.ConstExp(CONST))
    | VAR (AST.VarExp(VAR))
    | NUM (AST.NumExp(NUM))
    | FN LPAREN VAR COLON TYPE RPAREN COLON TYPE DARROW EXP (AST.fnExp(VAR, TYPE1, TYPE2, EXP))
    | FUN VAR LPAREN VAR COLON TYPE RPAREN COLON TYPE DARROW EXP (AST.FunExp(VAR1, VAR2, TYPE1, TYPE2, EXP))
    | LPAREN EXP EXP RPAREN (AST.AppExp(EXP1, EXP2))

TYPE: TYPE ARROW TYPE (AST.Arrow(TYPE1, TYPE2))
    | INT (AST.Int(INT))
    | BOOL (AST.Bool(BOOL))
