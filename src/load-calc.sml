structure CalcLrVals = CalcLrValsFun(structure Token = LrParser.Token)
structure CalcLex = CalcLexFun(structure Tokens = CalcLrVals.Tokens);
structure CalcParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = CalcLrVals.ParserData
     	       structure Lex = CalcLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "")
		in
		    CalcParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  CalcParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	

fun read_file (infile:string) =
   let 
    	val instream = TextIO.openIn infile
	fun loop instream =
		case TextIO.inputLine instream of
	             SOME line => line ^loop instream
    	    	   | NONE      => ""
    in
	 loop instream before TextIO.closeIn instream
    end
		
fun parse (lexer) =
    let val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = CalcParser.Stream.get lexer
    in
        if CalcParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, ""); result)
    end

(* val parseString = parse o stringToLexer o read_file *)
val parseString = parse o stringToLexer
(* val infile = valOf(TextIO.inputLine TextIO.stdIn); *)
(* val output = parseString(String.substring(infile,0,String.size(infille)-1)); *)
val infilestring  = read_file("f");

(*function to read file and store it in a list of characters*)
fun read_file2 (infile:string) = 
	let 
		fun next_string strm = (TextIO.inputAll strm);
		val stream = TextIO.openIn infile;
		val a = next_string stream

	in 
		String.explode(a)
	end;

(*function to convert list of characters to list of strings for conveniece of comparisosn later*)
fun CL_to_SL(ls) =
    case ls of
        [] => []
    |   x::xs =>
        String.str(x)::CL_to_SL(xs)
    ;

fun statements(ls,temp,opls) =
	case ls of
		[] => opls
	| 	x::xs => 
		if(x=";") then statements(xs,"",opls@[temp^x])
		else statements(xs,temp^x,opls);

fun parser(ls) =
	case ls of
		[] => []
	|	x::xs =>
		parseString(x)::parser(xs);

open EVALUATOR
val funenv = [];

fun evaluatorr(ls,fenv) =
	case ls of
		[] => []
	|	x::xs =>
		evalExp(x,[],fenv)::evaluatorr(xs,fenv);

fun storeFuns(ls,fenv) = 
	case ls of
		[] => fenv
	|	x::xs => 
			if(String.substring(x, 0, 3)="fun" orelse String.substring(x, 0, 4)="\nfun"  ) then 
				let val StmtExp (FunExp (f,v,t1,t2,e1)) = parseString(x) in storeFuns(xs,fenvAdd(f,v,e1,t1,t2,fenv)) end
			else
				storeFuns(xs,fenv);
(*fun write_file (outfilestring, outfile) = 
	let 
		val outstream = TextIO.openOut outfile;
		val check = TextIO.output(outstream,outfilestring)
    in
        TextIO.closeOut outstream
	end; 

fun ast2string(ls) =
	case ls of
		[] => ""
	|	x::xs => exp2ASTstring(x)^"\n"^ast2string(xs) ;
	
val wf = write_file(ast2string(asts),"out"); *)

(*val y = Option.valOf(x);*)
(*print(x);*)
(*val tc = typeCheckerr(asts);*)

val sf = storeFuns(statements(CL_to_SL(read_file2("f")),"",[]),funenv);
val asts = parser(statements(CL_to_SL(read_file2("f")),"",[]));
val asts_val = evaluatorr(asts,sf);
