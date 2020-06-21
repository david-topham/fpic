(* Basic Helpers *)

use "helpers.sml";

(* Types *)

use "types.sml";

(* Points and Vectors *)

use "points.sml";

(* The Environment *)

use "environment.sml";

(* The GraphicsContext *)

use "context.sml";

(* Code Generation in PSTricks *)

use "pstricks.sml";

(* The Basic Pictures *)

use "primitives.sml";

(* Interface-related stuff *)

use "regexp.sml";
use "latexboxes.sml";
use "text.sml";

(* The Not-so-basic Pictures (stuff not currently in the "core") *)

use "newprimitives.sml";

(* Draw Function *)

fun draw pic1 str = 
    let val ((_, tlY), _, _, _) = boundingBox pic1
	val (newPicFunc,newEnv)  = pic1 at (0.0,0.0)
	val os = TextIO.openOut (str^".tex")
	val initialOutput = "\\pspicture(0,0)" ^
	    (pointToString ((width (newPicFunc,newEnv)),
			    (height (newPicFunc,newEnv)))) ^ "\n"
    in (TextIO.outputSubstr (os, Substring.full (initialOutput ^ (newPicFunc
		  (((1.0, 0.0, 0.0), (0.0, 1.0, 0.0)),[("linecolor","0 0 0"),
			       ("fillcolor","0 0 0")]))
	       ^ "\n\\endpspicture\n"));
        TextIO.closeOut os)
    end;
    
