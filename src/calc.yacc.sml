functor CalcLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Calc_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* calc.yacc *)
(* User  declarations *)

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\015\000\002\000\014\000\003\000\026\000\004\000\025\000\
\\005\000\024\000\006\000\023\000\007\000\022\000\008\000\021\000\
\\009\000\013\000\010\000\020\000\011\000\012\000\012\000\019\000\
\\013\000\018\000\014\000\017\000\015\000\052\000\016\000\011\000\
\\017\000\010\000\020\000\009\000\024\000\008\000\031\000\007\000\
\\032\000\006\000\000\000\
\\001\000\001\000\015\000\002\000\014\000\009\000\013\000\011\000\012\000\
\\016\000\011\000\017\000\010\000\020\000\009\000\024\000\008\000\
\\031\000\007\000\032\000\006\000\000\000\
\\001\000\001\000\027\000\000\000\
\\001\000\001\000\047\000\000\000\
\\001\000\001\000\050\000\000\000\
\\001\000\001\000\053\000\000\000\
\\001\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\007\000\022\000\008\000\021\000\010\000\020\000\012\000\019\000\
\\013\000\018\000\014\000\017\000\015\000\058\000\000\000\
\\001\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\007\000\022\000\008\000\021\000\010\000\020\000\012\000\019\000\
\\013\000\018\000\014\000\017\000\022\000\064\000\000\000\
\\001\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\007\000\022\000\008\000\021\000\010\000\020\000\012\000\019\000\
\\013\000\018\000\014\000\017\000\025\000\048\000\000\000\
\\001\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\007\000\022\000\008\000\021\000\010\000\020\000\012\000\019\000\
\\013\000\018\000\014\000\017\000\026\000\063\000\000\000\
\\001\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\007\000\022\000\008\000\021\000\010\000\020\000\012\000\019\000\
\\013\000\018\000\014\000\017\000\027\000\073\000\000\000\
\\001\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\007\000\022\000\008\000\021\000\010\000\020\000\012\000\019\000\
\\013\000\018\000\014\000\017\000\028\000\016\000\000\000\
\\001\000\015\000\068\000\029\000\067\000\000\000\
\\001\000\015\000\070\000\029\000\067\000\000\000\
\\001\000\016\000\028\000\000\000\
\\001\000\016\000\046\000\000\000\
\\001\000\018\000\000\000\000\000\
\\001\000\019\000\057\000\000\000\
\\001\000\021\000\049\000\000\000\
\\001\000\023\000\031\000\000\000\
\\001\000\029\000\067\000\033\000\077\000\000\000\
\\001\000\029\000\067\000\033\000\078\000\000\000\
\\001\000\030\000\054\000\000\000\
\\001\000\030\000\059\000\000\000\
\\001\000\030\000\072\000\000\000\
\\001\000\030\000\074\000\000\000\
\\001\000\034\000\062\000\035\000\061\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\001\000\015\000\002\000\014\000\009\000\013\000\011\000\012\000\
\\016\000\011\000\017\000\010\000\020\000\009\000\024\000\008\000\
\\031\000\007\000\032\000\006\000\000\000\
\\086\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\007\000\022\000\008\000\021\000\010\000\020\000\012\000\019\000\
\\013\000\018\000\014\000\017\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\007\000\022\000\008\000\021\000\010\000\020\000\013\000\018\000\
\\014\000\017\000\000\000\
\\090\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\007\000\022\000\008\000\021\000\010\000\020\000\013\000\018\000\
\\014\000\017\000\000\000\
\\091\000\003\000\026\000\004\000\025\000\005\000\024\000\013\000\018\000\
\\014\000\017\000\000\000\
\\092\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\008\000\021\000\013\000\018\000\014\000\017\000\000\000\
\\093\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\013\000\018\000\014\000\017\000\000\000\
\\094\000\004\000\025\000\000\000\
\\095\000\004\000\025\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\003\000\026\000\004\000\025\000\005\000\024\000\000\000\
\\099\000\003\000\026\000\004\000\025\000\005\000\024\000\000\000\
\\100\000\003\000\026\000\004\000\025\000\005\000\024\000\013\000\018\000\
\\014\000\017\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\007\000\022\000\008\000\021\000\010\000\020\000\012\000\019\000\
\\013\000\018\000\014\000\017\000\000\000\
\\106\000\003\000\026\000\004\000\025\000\005\000\024\000\006\000\023\000\
\\007\000\022\000\008\000\021\000\010\000\020\000\012\000\019\000\
\\013\000\018\000\014\000\017\000\000\000\
\\107\000\000\000\
\\108\000\029\000\067\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\"
val actionRowNumbers =
"\001\000\028\000\027\000\011\000\
\\002\000\014\000\001\000\019\000\
\\047\000\001\000\001\000\001\000\
\\049\000\048\000\030\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\015\000\003\000\008\000\
\\018\000\004\000\000\000\045\000\
\\046\000\029\000\044\000\043\000\
\\035\000\034\000\038\000\037\000\
\\036\000\040\000\041\000\039\000\
\\005\000\022\000\001\000\001\000\
\\017\000\006\000\032\000\023\000\
\\026\000\009\000\007\000\001\000\
\\052\000\026\000\012\000\055\000\
\\054\000\001\000\042\000\031\000\
\\013\000\026\000\024\000\010\000\
\\025\000\053\000\026\000\033\000\
\\026\000\020\000\021\000\001\000\
\\001\000\050\000\051\000\016\000"
val gotoT =
"\
\\001\000\003\000\002\000\079\000\004\000\002\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\027\000\000\000\
\\003\000\028\000\000\000\
\\000\000\
\\001\000\030\000\000\000\
\\001\000\031\000\000\000\
\\001\000\032\000\000\000\
\\000\000\
\\000\000\
\\001\000\003\000\004\000\033\000\005\000\001\000\000\000\
\\001\000\034\000\000\000\
\\001\000\035\000\000\000\
\\001\000\036\000\000\000\
\\001\000\037\000\000\000\
\\001\000\038\000\000\000\
\\001\000\039\000\000\000\
\\001\000\040\000\000\000\
\\001\000\041\000\000\000\
\\001\000\042\000\000\000\
\\001\000\043\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\049\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\053\000\000\000\
\\001\000\054\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\058\000\000\000\
\\000\000\
\\000\000\
\\001\000\063\000\000\000\
\\000\000\
\\006\000\064\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\067\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\069\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\073\000\000\000\
\\000\000\
\\006\000\074\000\000\000\
\\000\000\
\\000\000\
\\001\000\077\000\000\000\
\\001\000\078\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 80
val numrules = 29
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | BOOL of unit ->  (string) | INT of unit ->  (string)
 | CONST of unit ->  (string) | NUM of unit ->  (int)
 | VAR of unit ->  (string) | TYPE of unit ->  (AST.typ)
 | STMT of unit ->  (AST.exp) | PRGM of unit ->  (AST.exp)
 | DECL of unit ->  (AST.decl) | START of unit ->  (AST.exp)
 | EXP of unit ->  (AST.exp)
end
type svalue = MlyValue.svalue
type result = AST.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 17) => true | _ => false
val showTerminal =
fn (T 0) => "VAR"
  | (T 1) => "NUM"
  | (T 2) => "PLUS"
  | (T 3) => "TIMES"
  | (T 4) => "MINUS"
  | (T 5) => "XOR"
  | (T 6) => "OR"
  | (T 7) => "AND"
  | (T 8) => "NEGATE"
  | (T 9) => "IMPLIES"
  | (T 10) => "NOT"
  | (T 11) => "EQUALS"
  | (T 12) => "LESSTHAN"
  | (T 13) => "GREATERTHAN"
  | (T 14) => "RPAREN"
  | (T 15) => "LPAREN"
  | (T 16) => "CONST"
  | (T 17) => "EOF"
  | (T 18) => "EQ"
  | (T 19) => "LET"
  | (T 20) => "IN"
  | (T 21) => "END"
  | (T 22) => "VAL"
  | (T 23) => "IF"
  | (T 24) => "THEN"
  | (T 25) => "ELSE"
  | (T 26) => "FI"
  | (T 27) => "TERM"
  | (T 28) => "ARROW"
  | (T 29) => "COLON"
  | (T 30) => "FN"
  | (T 31) => "FUN"
  | (T 32) => "DARROW"
  | (T 33) => "INT"
  | (T 34) => "BOOL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.PRGM PRGM1, PRGM1left, PRGM1right)) :: 
rest671)) => let val  result = MlyValue.START (fn _ => let val  (PRGM
 as PRGM1) = PRGM1 ()
 in (PRGM)
end)
 in ( LrTable.NT 1, ( result, PRGM1left, PRGM1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.STMT STMT1, STMT1left, STMT1right)) :: 
rest671)) => let val  result = MlyValue.PRGM (fn _ => let val  (STMT
 as STMT1) = STMT1 ()
 in (STMT)
end)
 in ( LrTable.NT 3, ( result, STMT1left, STMT1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.PRGM PRGM1, _, PRGM1right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.STMT (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  PRGM1 = PRGM1 ()
 in (AST.StmtExp(EXP))
end)
 in ( LrTable.NT 4, ( result, EXP1left, PRGM1right), rest671)
end
|  ( 3, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.EXP EXP1, 
EXP1left, _)) :: rest671)) => let val  result = MlyValue.STMT (fn _ =>
 let val  (EXP as EXP1) = EXP1 ()
 in (AST.StmtExp(EXP))
end)
 in ( LrTable.NT 4, ( result, EXP1left, TERM1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.VAR VAR1, _, _)) :: ( _, ( _, VAL1left, _)) :: rest671)) =>
 let val  result = MlyValue.DECL (fn _ => let val  (VAR as VAR1) = 
VAR1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (AST.ValDecl(VAR, EXP))
end)
 in ( LrTable.NT 2, ( result, VAL1left, EXP1right), rest671)
end
|  ( 5, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (AST.BracketExp(EXP))
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 6, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.EXP EXP3, _, _))
 :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP 
EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result
 = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 val  EXP3 = EXP3 ()
 in (AST.ItefExp(EXP1, EXP2, EXP3))
end)
 in ( LrTable.NT 0, ( result, IF1left, FI1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Implies, EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Equals, EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Xor, EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Or, EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.And, EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Plus, EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Minus, EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Times, EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 15, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)
) :: _ :: ( _, ( MlyValue.DECL DECL1, _, _)) :: ( _, ( _, LET1left, _)
) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (
DECL as DECL1) = DECL1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (AST.LetExp(DECL, EXP))
end)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Lessthan, EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Greaterthan, EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ =>
 let val  (EXP as EXP1) = EXP1 ()
 in (AST.UnExp(AST.Not, EXP))
end)
 in ( LrTable.NT 0, ( result, NOT1left, EXP1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
NEGATE1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _
 => let val  (EXP as EXP1) = EXP1 ()
 in (AST.UnExp(AST.Negate, EXP))
end)
 in ( LrTable.NT 0, ( result, NEGATE1left, EXP1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (CONST
 as CONST1) = CONST1 ()
 in (AST.ConstExp(CONST))
end)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.VAR VAR1, VAR1left, VAR1right)) :: rest671)
) => let val  result = MlyValue.EXP (fn _ => let val  (VAR as VAR1) = 
VAR1 ()
 in (AST.VarExp(VAR))
end)
 in ( LrTable.NT 0, ( result, VAR1left, VAR1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.EXP (fn _ => let val  (NUM as NUM1) = 
NUM1 ()
 in (AST.NumExp(NUM))
end)
 in ( LrTable.NT 0, ( result, NUM1left, NUM1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.TYPE TYPE2, _, _)) :: _ :: _ :: ( _, ( MlyValue.TYPE TYPE1, _
, _)) :: _ :: ( _, ( MlyValue.VAR VAR1, _, _)) :: _ :: ( _, ( _, 
FN1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ =>
 let val  (VAR as VAR1) = VAR1 ()
 val  TYPE1 = TYPE1 ()
 val  TYPE2 = TYPE2 ()
 val  (EXP as EXP1) = EXP1 ()
 in (AST.fnExp(VAR, TYPE1, TYPE2, EXP))
end)
 in ( LrTable.NT 0, ( result, FN1left, EXP1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.TYPE TYPE2, _, _)) :: _ :: _ :: ( _, ( MlyValue.TYPE TYPE1, _
, _)) :: _ :: ( _, ( MlyValue.VAR VAR2, _, _)) :: _ :: ( _, ( 
MlyValue.VAR VAR1, _, _)) :: ( _, ( _, FUN1left, _)) :: rest671)) =>
 let val  result = MlyValue.EXP (fn _ => let val  VAR1 = VAR1 ()
 val  VAR2 = VAR2 ()
 val  TYPE1 = TYPE1 ()
 val  TYPE2 = TYPE2 ()
 val  (EXP as EXP1) = EXP1 ()
 in (AST.FunExp(VAR1, VAR2, TYPE1, TYPE2, EXP))
end)
 in ( LrTable.NT 0, ( result, FUN1left, EXP1right), rest671)
end
|  ( 25, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP2, _,
 _)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1
 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.AppExp(EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.TYPE TYPE2, _, TYPE2right)) :: _ :: ( _, ( 
MlyValue.TYPE TYPE1, TYPE1left, _)) :: rest671)) => let val  result = 
MlyValue.TYPE (fn _ => let val  TYPE1 = TYPE1 ()
 val  TYPE2 = TYPE2 ()
 in (AST.Arrow(TYPE1, TYPE2))
end)
 in ( LrTable.NT 5, ( result, TYPE1left, TYPE2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.TYPE (fn _ => let val  (INT as INT1) =
 INT1 ()
 in (AST.Int(INT))
end)
 in ( LrTable.NT 5, ( result, INT1left, INT1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.BOOL BOOL1, BOOL1left, BOOL1right)) :: 
rest671)) => let val  result = MlyValue.TYPE (fn _ => let val  (BOOL
 as BOOL1) = BOOL1 ()
 in (AST.Bool(BOOL))
end)
 in ( LrTable.NT 5, ( result, BOOL1left, BOOL1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Calc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun VAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VAR (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun DARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun BOOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.BOOL (fn () => i),p1,p2))
end
end
