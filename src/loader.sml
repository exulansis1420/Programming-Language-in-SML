(* loader.sml *)
CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "ast.sml";
use "evaluator.sml";
use "calc.yacc.sig";
use "calc.yacc.sml";
use "calc.lex.sml";
Control.Print.printLength := 10000; (* set printing parameters so that *)
Control.Print.printDepth := 10000; (* we’ll see all details *)
Control.Print.stringDepth := 10000; (* and strings *)
use "load-calc.sml";
