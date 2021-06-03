(* ast.sml *)
structure AST =
struct
 
type var = string

datatype binop = Plus | Minus | Times | Eq | Xor | Or | And | Implies | Equals | Lessthan | Greaterthan 

datatype unop = Negate | Not

datatype typ = Arrow of typ * typ 
            |  Int of string
            |  Bool of string

fun bop2string(bop:binop):string =
    case bop of
        Plus => "PLUS"
    |   Minus => "MINUS"
    |   Times => "TIMES"
    |   Eq => "EQ"
    |   Xor => "XOR"
    |   Or => "OR"
    |   And => "AND"
    |   Implies => "IMPLIES"
    |   Equals => "EQUALS"
    |   Lessthan => "LESSTHAN"
    |   Greaterthan => "GREATERTHAN";

fun uop2string(uop:unop):string =
    case uop of
        Not => "NOT"
    |   Negate => "NEGATE";

fun uop2string(uop:unop):string =
    case uop of
        Negate => "NEGATE"
    |   Not => "NOT";

datatype decl = ValDecl of var * exp
and exp = NumExp of int
        | StringExp of string
        | BracketExp of exp 
        | VarExp of var
        | BinExp of binop * exp * exp
        | DeclExp of decl
        | UnExp of unop * exp
        | LetExp of decl * exp
        | ItefExp of exp * exp * exp 
        | ConstExp of string
        | StmtExp of exp
        | fnExp of var * typ * typ * exp
        | FunExp of var * var * typ * typ * exp
        | AppExp of exp * exp

fun typ2string(t:typ):string =
    case t of
            Arrow(t1,t2) => typ2string(t1)^"->"^typ2string(t2)
        |   Int(s) => "int"
        |   Bool(s) => "bool"


datatype value = IntVal of int
	           | BoolVal of bool
               | FunDecl of string
               | FunVal of var * exp * typ * typ
               | error of unit

fun exp2string (e:exp):string =
    case e of
          NumExp(i) => Int.toString(i)
        | StringExp (s) => s
        | BracketExp (e1) => "("^exp2string(e1)^")"
        | VarExp (v) => v
        | BinExp (b, e1, e2) => exp2string(e1)^" "^bop2string(b)^" "^exp2string(e2)
        | DeclExp (ValDecl(x, e1)) => "val "^x^" = "^exp2string(e1)
        | UnExp (u, e1) => uop2string(u)^" "^exp2string(e1)
        | LetExp (ValDecl(x, e1), e2) => "let val "^x^" = "^exp2string(e1)^" in "^exp2string(e2)
        | ItefExp (e1, e2, e3) => "if "^exp2string(e1)^" then "^exp2string(e2)^" else "^exp2string(e3)^" fi "
        | ConstExp (s) => s
        | StmtExp (e1) => exp2string(e1)
        | fnExp (v,t1,t2,e1) => "fn ("^v^":"^typ2string(t1)^"):"^typ2string(t2)^" => "^exp2string(e1)
        | FunExp (v1,v2,t1,t2,e1) => "fun "^v1^"("^v2^":"^typ2string(t1)^"):"^typ2string(t2)^" => "^exp2string(e1)
        | AppExp (e1, e2) => "("^exp2string(e1)^" "^exp2string(e2)^")";

(*fun typ2AST (t:typ):string =
    case t of
            Arrow(t1,t2) => "Arrow ("^typ2AST(t1)^","^typ2AST(t2)^")"
        |   Int(s) => "int"
        |   Bool(s) => "bool"

fun exp2ASTstring (e:exp):string =
    case e of
          NumExp(i) => "NumExp "^Int.toString(i)
        | StringExp (s) => "StringExp "^s
        | BracketExp (e1) => "BracketExp ("^exp2ASTstring(e1)^")"
        | VarExp (v) => "VarExp "^v
        | BinExp (b, e1, e2) => "BinExp ("^bop2string(b)^","^exp2ASTstring(e1)^","^exp2ASTstring(e2)^")"
        | DeclExp (ValDecl(x, e1)) => "ValDecl ("^"VarExp "^x^","^exp2ASTstring(e1)^")"
        | UnExp (u, e1) => "UnExp ("^uop2string(u)^","^exp2ASTstring(e1)^")"  
        | LetExp (ValDecl(x, e1), e2) => "LetExp( " ^ "ValDecl ("^"VarExp "^x^","^exp2ASTstring(e1)^")" ^ "," ^ exp2ASTstring(e2) ^ ")"
        | ItefExp (e1, e2, e3) => "ItefExp (" ^ exp2ASTstring(e1) ^ "," ^ exp2ASTstring(e2) ^ "," ^ exp2ASTstring(e3) ^ ")"
        | ConstExp (s) => "ConstExp "^s
        | StmtExp (e1) => "StmtExp ("^exp2ASTstring(e1)^")"
        | fnExp (v,t1,t2,e1) => "fnExp (" ^ "VarExp" ^ v ^ "," ^ typ2AST(t1) ^ "," ^ typ2AST(t2) ^ ")" ^ exp2ASTstring(e1)^ ")"
        | FunExp (v1,v2,t1,t2,e1) => "fun "^v1^"("^v2^":"^typ2string(t1)^"):"^typ2string(t2)^" => "^exp2string(e1)
        | AppExp (v1, e1) => "AppExp (" ^ "VarExp " ^ v1 ^ "," ^ exp2ASTstring(e1) ^  ")"*)
    
datatype value = IntVal of int
	           | BoolVal of bool
               | FunDecl of string
               | FunVal of var * exp * typ * typ
               | error of unit

type environment = (var * value) list
type fenvironment = (var * var * exp * typ * typ) list

fun envAdd_iter(variable,v,env,nenv, p) =
    case env of
        []=> if(p>0) then nenv else (variable,v)::nenv
    |    (x,vx)::ls => if (variable = x) then envAdd_iter(variable,v,ls,(variable,v)::nenv,p+1) else envAdd_iter(variable,v,ls,(x,vx)::nenv,p);

fun fenvAdd_iter(f,x,e,t1,t2,fenv,nfenv,p) =
    case fenv of
        []=> if(p>0) then nfenv else (f,x,e,t1,t2)::nfenv
    |    (ff,xx,ee,tt1,tt2)::ls => if (f = ff) then fenvAdd_iter(f,x,e,t1,t2,ls,(f,x,e,t1,t2)::nfenv,p+1) else fenvAdd_iter(f,x,e,t1,t2,ls,(ff,xx,ee,tt1,tt2)::nfenv,p);

fun envAdd (variable:var, v:value, env:environment) = envAdd_iter(variable,v,env,[],0);

fun fenvAdd (f,x,e,t1,t2,fenv) = fenvAdd_iter(f,x,e,t1,t2,fenv,[],0);

fun envLookup (variable:var, env:environment):value =
    case List.find(fn (x, _) => x = variable) env of
				        SOME (x, v)   => v
				    |   NONE => error(print(""));

fun fenvLookup (f:var,fenv:fenvironment):value =
    case List.find(fn (x, _,_,_,_) => x = f) fenv of
				        SOME (x, v,e,t1,t2)   => FunVal (v,e,t1,t2)
				    |   NONE => error(print(""));

end