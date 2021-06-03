structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 1
  val col = ref 1
  val eof = fn () => Tokens.EOF(!pos, !pos)
 	val error = fn (token, col:int, line:int) => TextIO.output (TextIO.stdOut,"Unknown Token:" ^ (Int.toString line) ^ ":"^ (Int.toString col) ^ ":" ^ token ^ ", ")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  
%%
%header (functor CalcLexFun(structure Tokens:Calc_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{digit}+ => (print ("NUM " ^ "\"" ^ yytext ^ "\", "); col := (!col) + String.size(yytext) ; Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      !pos, !pos));
"AND" 		=> (print ("AND " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 3 ; Tokens.AND( !pos, !pos));
"OR" 		=> (print ("OR " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 2 ; Tokens.OR( !pos, !pos));
"XOR" 		=> (print ("XOR " ^ "\"" ^ yytext ^ "\" "); col := (!col) + 3 ; Tokens.XOR( !pos, !pos));
"EQUALS" 	=> (print ("EQUALS " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 6 ; Tokens.EQUALS( !pos, !pos));
"NOT" 		=> (print ("NOT " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 3 ; Tokens.NOT( !pos, !pos));
"IMPLIES" 	=> (print ("IMPLIES " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 7 ; Tokens.IMPLIES( !pos, !pos));
";" 		=> (print ("TERM " ^ "\"" ^ yytext ^ "\"\n"); col := (!col) + 1 ; Tokens.TERM( !pos, !pos));
"TRUE" 		=> (print ("CONST " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 4 ; Tokens.CONST( yytext,!pos,!pos));
"FALSE"		=> (print ("CONST " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 5 ; Tokens.CONST( yytext, !pos, !pos));
"if" 		=> (print ("IF " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 2 ; Tokens.IF( !pos, !pos));
"else" 		=> (print ("ELSE " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 4 ; Tokens.ELSE( !pos, !pos));
"then" 		=> (print ("THEN " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 4 ; Tokens.THEN( !pos, !pos));
"fi" 		=> (print ("FI " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 2 ; Tokens.FI( !pos, !pos));
"PLUS"      => (print ("PLUS " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 4 ; Tokens.PLUS(!pos,!pos));
"TIMES"      => (print ("TIMES " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 5 ; Tokens.TIMES(!pos,!pos));
"("      => (print ("LPAREN " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 1 ; Tokens.LPAREN(!pos,!pos));
")"      => (print ("RPAREN " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 1 ; Tokens.RPAREN(!pos,!pos));
"MINUS"      => (print ("MINUS " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 5 ; Tokens.MINUS(!pos,!pos));
"let"    => (print ("LET " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 3 ; Tokens.LET(!pos,!pos));
"in"     => (print ("IN " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 2 ; Tokens.IN(!pos,!pos));
"end"    => (print ("END " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 3 ; Tokens.END(!pos,!pos));
"val"    => (print ("VAL " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 3 ; Tokens.VAL(!pos,!pos));
"="     => (print ("EQ " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 1 ; Tokens.EQ(!pos,!pos));
"LESSTHAN"  => (print ("LESSTHAN " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 8 ; Tokens.LESSTHAN(!pos,!pos));
"GREATERTHAN" => (print ("GREATERTHAN " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 11 ; Tokens.GREATERTHAN(!pos,!pos));
"NEGATE"    => (print ("NEGATE " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 6 ; Tokens.NEGATE(!pos,!pos));
"fn"        => (print ("FN " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 2 ; Tokens.FN(!pos,!pos));
"fun"        => (print ("FUN " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 3 ; Tokens.FUN(!pos,!pos));
"int"       => (print ("INT " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 3 ; Tokens.INT( yytext,!pos,!pos));
"bool"      => (print ("BOOL " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 4 ; Tokens.BOOL(yytext, !pos,!pos));
"->"        => (print ("ARROW " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 2 ; Tokens.ARROW(!pos,!pos));
"=>"        => (print ("DARROW " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 6 ; Tokens.DARROW(!pos,!pos));
":"        => (print ("COLON " ^ "\"" ^ yytext ^ "\", "); col := (!col) + 1 ; Tokens.COLON(!pos,!pos));
{alpha}+ => (print ("VAR " ^ "\"" ^ yytext ^ "\", "); col := (!col) + String.size(yytext) ; Tokens.VAR(yytext, !pos,!pos));
.			=>((error ( yytext, !col, !pos);lex());lex());