structure EVALUATOR  =
struct
open AST 

val brokenTypes = Fail "Error in evaluation!";

fun convertBool(s:string) =
    if(s="TRUE") then true
    else false;

fun domain(bop):string =
    case bop of
        Plus => "int * int"
    |   Minus => "int * int"
    |   Times => "int * int"
    |   Xor => "bool * bool"
    |   Or => "bool * bool"
    |   And => "bool * bool"
    |   Implies => "bool * bool"
    |   Equals => "'Z * 'Z"
    |   Eq=> "'Z * 'Z"
    |   Lessthan => "int * int"
    |   Greaterthan => "int * int";
    
fun domainu(uop) :string =
    case uop of
        Not => "bool"
    |   Negate => "int";

fun varTypLookUp(v, env,fenv) =
    case  (envLookup(v,env)) of
        IntVal(i1) => "int"
    |   BoolVal(b1) => "bool"
    |   _ => let val FunVal (x,ee,t1,t2) = fenvLookup(v,fenv) in typ2string(t1)^"->"^typ2string(t2) end;




fun evalExp(e:exp, env:environment, fenv:fenvironment):value =
    case e of
	    NumExp i              => IntVal i
      | StringExp s          => FunDecl s
      | BracketExp be        => evalExp(be,env,fenv)
      | UnExp (u, e1)         => evalUnExp(u,e1,e,env,fenv)
      | VarExp x             => if(envLookup (x, env)=error()) then fenvLookup(x,fenv) else envLookup(x,env)		  
      | BinExp (b, e1, e2)   => evalBinExp(b, e1, e2,e, env, fenv)
      | ItefExp (e1, e2, e3) => evalItefExp(e1,e2,e3,e,env, fenv)
      | ConstExp s           => BoolVal (convertBool(s))
      | StmtExp e            => evalExp(e,env, fenv) 
      | LetExp(ValDecl(f, fnExp(v,t1,t2,e1) ), e2)  => evalExp(e2, env, fenvAdd (f,v, e1,t1,t2, fenv))
      | FunExp (v1,v2,t1,t2,e1) => evalExp(StringExp v1,env, fenvAdd(v1,v2,e1,t1,t2,fenv))
      | AppExp(AppExp(ee1,ee2),e2) => let val (tempf,envn,fenvn) = reduce(AppExp(ee1,ee2),env,fenv) in evalFun(tempf,e2,envn,fenvn,e) end
      | AppExp (VarExp(f),e2) => let val v1 = f in evalFun (v1,e2,env,fenv,e) end
      | LetExp(ValDecl(x, e1), e2)  =>
            let
                val v1 = evalExp (e1, env, fenv)
            in
                evalExp(e2, envAdd (x, v1, env), fenv)
            end		   
and
reduce(e,env,fenv) = 
    let val AppExp(ee1,ee2) = e in
        case ee1 of
            VarExp(f) => 
                let 
                    val FunVal(vf,ef,t1f,t2f) = fenvLookup(f,fenv)
                    val fnExp(va,t1a,t2a,e1a) = ef
                    val tempf:var =  "anon"
                    val envn = envAdd(vf,evalExp(ee2,env,fenv),env)
                    val fenvn = fenvAdd(tempf,va,e1a,t1a,t2a,fenv)
                in 
                    ("anon", envn, fenvn)
                end
           | AppExp(eee1,eee2) => 
                let 
                    val (tempf,envn,fenvn) = reduce(ee1,env,fenv)
                    val FunVal(vf,ef,t1f,t2f) = fenvLookup(tempf,fenvn)
                    val fnExp(va,t1a,t2a,e1a) = ef
                    val tempff:var =  "anon"
                    val envnn = envAdd(vf,evalExp(ee2,envn,fenvn),envn)
                    val fenvnn = fenvAdd(tempff,va,e1a,t1a,t2a,fenvn)
                in
                    ("anon", envnn, fenvnn)
                end

    end
and
typeLookUp(e,env,fenv):string =
    case e of
        UnExp (Not, e1)         => "bool"
      | UnExp (Negate, e1)      => 	"int"  
      | BinExp (Plus, e1, e2)   =>  "int"
      | BinExp (Minus, e1, e2)   =>  "int"
      | BinExp (Times, e1, e2)   =>  "int"
      | BinExp (Lessthan, e1, e2)   => "bool"
      | BinExp (Greaterthan, e1, e2)   => "bool"
      | BinExp (And, e1, e2)    => "bool"
      | BinExp (Or, e1, e2)    => "bool"
      | BinExp (Xor, e1, e2)    => "bool"
      | BinExp (Implies, e1, e2)  =>  "bool"
      | BinExp (Equals, e1, e2) => "bool"
      | ItefExp (e1, e2, e3) => typeLookUp(e2,env,fenv)
      | NumExp (i) => "int"
      | ConstExp (s) => "bool"
      | StmtExp (e1) => typeLookUp(e1,env,fenv)
      | VarExp (v) => varTypLookUp(v,env,fenv)
      | LetExp (ValDecl(x, e1), e2) => typeLookUp(e2,env,fenv)
      | BracketExp(e1) => typeLookUp(e1,env,fenv)
      | fnExp (v,t1,t2,e1) => typ2string(t1)^"->"^typ2string(t2)
      | FunExp (v1,v2,t1,t2,e1) => typ2string(t1)^"->"^typ2string(t2)
      | AppExp (AppExp(ee1,ee2), e2) => "int->inht"
      | AppExp(VarExp(f),e2) => let val FunVal (x,ee,t1,t2) = fenvLookup(f,fenv) in typ2string(t2) end
and
bopMismatch(b, e1, e2, e,env,fenv) = 
    if ((b = Equals) orelse (b = Eq)) then if (not (typeLookUp(e1,env,fenv) = typeLookUp(e2,env,fenv))) then "\nType mismatch in: " ^ exp2string(e) ^"\nOperator: "^bop2string(b)^ "\nOperator domain: "^domain(b)^"\nOperand domain: "^typeLookUp(e1,env,fenv)^" * "^typeLookUp(e2,env,fenv)^"\n\n" else ""
    else if(not (typeLookUp(e1,env,fenv)^" * "^typeLookUp(e2,env,fenv) = domain(b))) then "\nType mismatch in: " ^ exp2string(e) ^"\nOperator: "^bop2string(b)^ "\nOperator domain: "^domain(b)^"\nOperand domain: "^typeLookUp(e1,env,fenv)^" * "^typeLookUp(e2,env,fenv)^"\n\n"
    else ""
and
uopMismatch(u, e1,  e,env,fenv) =
    if(not (typeLookUp(e1,env,fenv) = domainu(u))) then "\nType mismatch in: " ^ exp2string(e) ^"\nOperator: "^uop2string(u)^ "\nOperator domain: "^domainu(u)^"\nOperand domain: "^typeLookUp(e1,env,fenv)^"\n\n"
    else ""
and
itefMismatch (e1,e2,e3,e,env,fenv) =
    if(not (typeLookUp(e1,env,fenv)="bool") orelse not(typeLookUp(e2,env,fenv)=typeLookUp(e3,env,fenv))) then "\nType mismatch in: " ^ exp2string(e) ^"\nOperator: if then else fi"^ "\nOperator domain: bool * 'Z * 'Z\nOperand domain: "^typeLookUp(e1,env,fenv)^" * "^typeLookUp(e2,env,fenv)^" * "^typeLookUp(e3,env,fenv)^"\n\n"
    else ""
and
evalBinExp(b:binop, e1:exp, e2:exp, e,env:environment, fenv):value =
case (b, evalExp(e1, env, fenv), evalExp(e2, env, fenv),e)  of
      (Plus, IntVal i1, IntVal i2, e) => IntVal (i1+i2)
  |   (Minus, IntVal i1, IntVal i2, e) => IntVal (i1-i2)
  |   (Times, IntVal i1, IntVal i2, e) => IntVal (i1*i2)
  |   (Equals, IntVal i1, IntVal i2, e)  => BoolVal (i1 = i2)
  |   (Equals, FunDecl s1, FunDecl s2, e) => BoolVal (s1 = s2)
  |   (Equals, BoolVal s1, BoolVal s2, e) => BoolVal (s1 = s2)
  |   (And, BoolVal b1, BoolVal b2,e) => BoolVal (b1 andalso b2)
  |   (Or, BoolVal b1, BoolVal b2,e) => BoolVal (b1 orelse b2)
  |   (Xor, BoolVal b1, BoolVal b2,e) => BoolVal ((b1 andalso (not b2)) orelse (b2 andalso (not b1)))
  |   (Implies, BoolVal b1, BoolVal b2,e) => BoolVal (not (b1 andalso (not b2)))
  |   (Lessthan, IntVal i1, IntVal i2,e) => BoolVal (i1<i2)
  |   (Greaterthan, IntVal i1, IntVal i2,e) => BoolVal (i1>i2)
  |   _  =>error (print(bopMismatch(b, e1, e2, e,env, fenv)))
and
evalItefExp(e1, e2, e3, e,env, fenv):value =
    let 
        val (ee1, env,fenv, ee) = (evalExp(e1, env,fenv), env, fenv, e) 
    in
        if(ee1 = BoolVal(true) andalso (typeLookUp(e2,env,fenv)=typeLookUp(e3,env,fenv)))  then evalExp(e2, env,fenv)
        else if(ee1 = BoolVal(false) andalso (typeLookUp(e2,env,fenv)=typeLookUp(e3,env,fenv))) then evalExp(e3, env,fenv)
        else error(print(itefMismatch (e1,e2,e3,e,env, fenv)))
    end
and
evalUnExp(u:unop,e1:exp,e,env:environment, fenv):value =
  case (u, evalExp(e1, env, fenv))  of
    (Negate, IntVal i1) => IntVal (0-i1)
  | (Not, BoolVal b1) => BoolVal (not b1)
  |   _  => error(print(uopMismatch(u,e1,e,env,fenv)))   
and
evalFun(f,e1,env,fenv,e) =
    let
        val FunVal (v,ee,t1,t2) = fenvLookup(f,fenv)
    in
        if(not ( typ2string(t1) = typeLookUp(e1,env,fenv))) then error (print("\nType mismatch in: " ^ exp2string(e) ^"\nOperator: "^"f"^ "\nOperator domain: "^typ2string(t1)^"\nOperand type: "^typeLookUp(e1,env,fenv)^"\n\n"))
        else 
            let
                val iner = evalExp(e1,env,fenv) (*hmmm*)
            in

                case iner of
                    IntVal i => 
                    evalExp(ee, envAdd(v,iner,env),fenv )
                |   BoolVal b => if(not (typ2string(t2) = "bool")) then error (print("\nType mismatch in: " ^ exp2string(e) ^"\nOperator: "^"f"^ "\nOperator range: "^typ2string(t2)^"\nOperand type: "^"bool"^"\n\n")) else evalExp(ee, envAdd(v,iner,env),fenv )
                |   FunVal (x,fe,tt1,tt2) => if(not (typ2string(t2) = typ2string(tt2))) then error (print("\nType mismatch in: " ^ exp2string(e) ^"\nOperator: "^"f"^ "\nOperator range: "^typ2string(t2)^"\nOperand type: "^typ2string(tt2)^"\n\n")) else evalExp(ee, env, fenvAdd(v,x,fe,tt1,tt2,fenv) )

            end
        
    end; 

end
