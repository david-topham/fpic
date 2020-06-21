fun printMatrix ((m1,m2,m3),(m4,m5,m6)) = 
    "\n\\code{ [" ^ (coordToString m1) ^ " " ^
    (coordToString m4) ^ " " ^
    (coordToString m2) ^ " " ^
    (coordToString m5) ^ " " ^ "}\n" ^
    "\\dim{" ^ (coordToString m3) ^ "} \\dim{" ^ (coordToString m6) ^ "}\n"
    ^ "\\code{ ] concat }\n";

fun colorprefix gc = 
    let val lineCommand = "\\newrgbcolor{lcolor}{" ^ 
	(getAttribute "linecolor" gc) ^ "}\n"
	val fillCommand = "\\newrgbcolor{fcolor}{" ^
	    (getAttribute "fillcolor" gc) ^ "}\n"
    in lineCommand ^ fillCommand 
    end;

fun handleAttrs gc = 
    let val (nmtx,nattrs) = addAttribute ("linecolor", "lcolor")
        (addAttribute ("fillcolor", "fcolor") gc)
	fun attrHelper [] = ""
	  | attrHelper [(var,value)] = var ^ "=" ^ value
	  | attrHelper ((var,value)::rst) = var ^ "=" ^ value 
	^ "," ^ (attrHelper rst)
    in "[" ^ attrHelper nattrs ^ "]" 
    end;

fun pswrapper (mtx,attrs) txt = 
    let val colorResult = colorprefix (mtx,attrs)
    in colorResult ^ "\\pscustom" ^ (handleAttrs (mtx,attrs)) ^ "{" ^ (printMatrix mtx) ^ txt ^ "}\n"
    end;

