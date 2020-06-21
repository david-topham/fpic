(* Colors *)

val red = (1.0,0.0,0.0);
val green = (0.0,1.0,0.0);
val blue = (0.0,0.0,1.0);
val white = (1.0,1.0,1.0);
val black = (0.0,0.0,0.0);
val magenta = (1.0,0.0,1.0);
val cyan = (0.0,1.0,1.0);
val lightGray = (0.7,0.7,0.7);
val darkGray = (0.4,0.4,0.4);
val yellow = (1.0,1.0,0.0);

(* Attributes *)

fun getAttribute attr (mtx,[]) = ""
  | getAttribute attr (mtx,((var,value)::rst)) = if (attr = var) then value else
    getAttribute attr (mtx,rst);

fun addAttribute (var,value) (mtx,attrs) = 
    let fun addHelper (var,value) [] = [(var,value)]
      | addHelper (var,value) ((x,y)::rst) = 
	if (var = x) then ((var,value)::rst) else (x,y)::(addHelper (var,value) rst)
    in (mtx,addHelper (var,value) attrs)
    end;

(* The Transformation Functions *)

fun matrixMult ((m1:real,m2,m3),(m4,m5,m6)) ((n1,n2,n3),(n4,n5,n6)) = 
  let val x1 = (m1*n1)+(m2*n4)
      val x2 = (m1*n2)+(m2*n5)
      val x3 = (m1*n3)+(m2*n6)+m3
      val x4 = (m4*n1)+(m5*n4)
      val x5 = (m4*n2)+(m5*n5)
      val x6 = (m4*n3)+(m5*n6)+m6
  in ((x1,x2,x3),(x4,x5,x6))
  end;

(* Transforms a point using the 3x3 matrix given (last row assumed) *)
fun transformPoint ((m1,m2,m3), (n1,n2,n3)) (x,y):Point =
    let val newX = (m1*x) + (m2*y) + m3
	val newY = (n1*x) + (n2*y) + n3
    in (newX, newY) end;

(* Transforms a named point *)
fun transformNamedPoint matrix (name,pnt) = 
    (name, transformPoint matrix pnt);

(* Transform the entire picture using the 3x3 matrix given *)
fun transformPicture matrix (pf, tree(n,pts,pics)) = 
    let val newPicFunc = fn (mtx, attrs) => 
	    pf (matrixMult mtx matrix, attrs)
    in (newPicFunc, tree(n, map (transformNamedPoint matrix) pts,
                            map (transformPicture matrix) pics))
    end;

infix 7 offsetBy;
infix 7 at;
infix 7 centeredAt;
infix 7 scale;
infix 7 scaleX;
infix 7 scaleY;
infix 7 scaleXY;
infix 7 rotate;
infix 7 scaleWithPoint;
infix 7 rotateWithPoint;
infix 7 withBoundingBox;
infix 7 withLineColor;
infix 7 withFillColor;
infix 7 withFillStyle;
infix 7 withArrowStyle;
infix 7 withLineStyle;
infix 7 withLineWidth;
infix 7 withCurvature;
infix 7 withAttribute;
infix 7 transformedWith;

(* Color Transformations *)
fun (pf, env) withLineColor (x:real,y:real,z:real) =
    let val newColor = colorToString (x,y,z) 
	val newPicFunc =
              fn gc => pf (addAttribute ("linecolor", newColor) gc) 
    in (newPicFunc, env)
    end;

fun (pf, env) withFillColor (x:real,y:real,z:real) =
    let val newColor = colorToString (x,y,z)
        val newPicFunc = fn gc => 
	    if (getAttribute "fillstyle" gc) = "" 
		then pf (addAttribute ("fillstyle", "solid")
                          (addAttribute ("fillcolor", newColor) gc))
	    else pf (addAttribute ("fillcolor",newColor) gc)
    in (newPicFunc, env)
    end;

fun (pf, env) withFillStyle r = 
    let val newPicFunc = fn gc => pf (addAttribute ("fillstyle", r) gc)
    in (newPicFunc, env)
    end;

fun (pf, env) withArrowStyle ar = 
    let val newPicFunc =
              fn gc => pf (addAttribute ("arrows", ar)
                              (addAttribute ("arrowlength", "2.5") gc))
    in (newPicFunc, env) 
    end;

fun (pf, env) withLineStyle r =
    let val newPicFunc = fn gc => pf (addAttribute ("linestyle", r) gc)
    in (newPicFunc, env)
    end;

fun (pf, env) withLineWidth r =
    let val newPicFunc =
               fn gc => pf (addAttribute ("linewidth", (coordToString r)) gc)
    in (newPicFunc, env)
    end;

fun (pf, env) withCurvature (n1,n2,n3) = 
    let val newPicFunc =
	fn gc => pf (addAttribute ("curvature",
		  	       (coordToString n1) ^ " " ^
			       (coordToString n2) ^ " " ^
			       (coordToString n3)) gc)
    in (newPicFunc, env)
    end;

fun (pf, env) withAttribute attr = 
    let val newPicFunc = fn gc => pf (addAttribute attr gc)
    in (newPicFunc, env)
    end;

fun aPicture withBoundingBox (nw, ne, sw, se) =
    namePts aPicture [("nw", nw), ("ne", ne), ("sw", sw), ("se", se),
		      ("c", midpoint nw se),
		      ("n", midpoint nw ne), ("e", midpoint ne se),
		      ("w", midpoint nw sw), ("s", midpoint sw se)];
	
fun aPicture offsetBy (x1, y1) =
    let val newMatrix = ((1.0, 0.0, x1), (0.0, 1.0, y1))
    in transformPicture newMatrix aPicture
    end;

fun mtxOffset mtx = transformPoint mtx (0.0,0.0);

fun aPicture scaleWithPoint ((x1,y1), (centerX, centerY)) =
    let val newMatrix = ((x1, 0.0, ~centerX*x1+centerX),
                         (0.0, y1, ~centerY*y1+centerY))
    in transformPicture newMatrix aPicture
    end;

fun aPicture scaleXY (x1,y1) =
    aPicture scaleWithPoint ((x1,y1), center aPicture);

fun mtxScale mtx = 
(
 print "mtxScale";
 print (pointToString  (transformPoint mtx (1.0,1.0)) );
  print (pointToString (mtxOffset mtx));
  (transformPoint mtx (1.0,1.0)) -- (mtxOffset mtx)
);

fun aPicture scaleX x = aPicture scaleXY (x, 1.0);
fun aPicture scaleY y = aPicture scaleXY (1.0, y);
fun aPicture scale s = aPicture scaleXY (s, s);

fun aPicture rotateWithPoint (degAngle, (centerX, centerY)) = 
    let val x = (degAngle / 360.0) * 6.28318
        val cosX = Math.cos x
        val sinX = Math.sin x
        val first = (~(centerX*cosX))+(centerY*sinX)+centerX
        val second = (~(centerX*sinX))-(centerY*cosX)+centerY
        val newMatrix = ((cosX, ~sinX, first), (sinX, cosX, second))
    in transformPicture newMatrix aPicture
    end;

fun aPicture rotate degAngle = 
    aPicture rotateWithPoint (degAngle, center aPicture);

fun mtxAngle mtx =
    let val (x,y) = (transformPoint mtx (1.0,0.0)) -- mtxOffset mtx
    in
(print (Real.toString x);print ", "; print (Real.toString y);
 (Math.atan (y/x)) * 57.2958279
)
    end;

fun aPicture at pnt1 = 
    let val (_, _, pnt2, _) = boundingBox aPicture
	val off = pnt1 -- pnt2
    in aPicture offsetBy off
    end;

fun aPicture centeredAt pnt1 = 
    let val off = pnt1 -- (center aPicture)
    in aPicture offsetBy off
    end;

fun aPicture transformedWith matrix = transformPicture aPicture matrix;
