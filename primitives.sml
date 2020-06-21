
(* The empty picture *)
val (emptyPicFunc, emptyEnv) = ((fn gc => ""),
				tree("", [("sw", (0.0, 0.0)),
					  ("se", (0.0, 0.0)),
					  ("nw", (0.0, 0.0)),
					  ("ne", (0.0, 0.0)),
					  ("c", (0.0, 0.0)),
					  ("n", (0.0, 0.0)),
					  ("s", (0.0, 0.0)),
					  ("e", (0.0, 0.0)),
					  ("w", (0.0, 0.0))], []));

val empty = (emptyPicFunc, emptyEnv);
    
(* A basic box *)
fun box width height = 
  let val picFunc = fn gc => pswrapper gc ("\\pspolygon" ^
      (handleAttrs gc) ^ 
      (pointToString (0.0,0.0)) ^ " " ^
      (pointToString (0.0,height)) ^ " " ^
      (pointToString (width,height)) ^ " " ^
      (pointToString (width,0.0)) ^ "\n")
      val env = tree("", [("sw", (0.0, 0.0)),
			  ("se", (width, 0.0)),
			  ("nw", (0.0, height)),
			  ("ne", (width, height)),
			  ("c", (width/2.0, height/2.0)),
			  ("w", (0.0, height/2.0)),
			  ("e", (width, height/2.0)),
			  ("n", (width/2.0, height)),
			      ("s", (width/2.0, 0.0))],
		     [])
  in (picFunc, env) end;

fun oval xrad yrad = 
    let val picFunc = fn gc => pswrapper gc ("\\psellipse" ^
	(handleAttrs gc) ^
	(pointToString (xrad, yrad)) ^ " (" ^
	(coordToString (distance (xrad, yrad) (0.0, yrad)))
	^ "," ^ (coordToString (distance (xrad, yrad) (xrad, 0.0)))
	^ ")\n")
	val xdiameter = xrad * 2.0
	val ydiameter = yrad * 2.0
	val env = tree("", [("sw", (0.0, 0.0)),
			    ("se", (xdiameter, 0.0)),
			    ("nw", (0.0, ydiameter)),
			    ("ne", (xdiameter, ydiameter)),
			    ("c", (xdiameter/2.0, ydiameter/2.0)),
			    ("w", (0.0, yrad)),
			    ("e", (xdiameter, yrad)),
			    ("n", (xrad, ydiameter)),
			    ("s", (xrad, 0.0))],
		       [])
    in (picFunc, env) end;  

fun line pt1 pt2  = 
    let val picFunc = fn gc => pswrapper gc ("\\psline" ^
      (handleAttrs gc) ^ (pointToString pt1) ^
	" " ^ (pointToString pt2) ^ "\n")
	val (nw, ne, sw, se) = 
	    computeBoundingBoxFromPoints [pt1, pt2]
	val ptList = [("start", pt1), ("end", pt2),
		      ("midpoint", midpoint pt1 pt2),
		      ("nw", nw),
		      ("ne", ne),
		      ("sw", sw),
		      ("se", se),
		      ("c", midpoint pt1 pt2),
		      ("n", midpoint nw ne),
		      ("s", midpoint sw se),
		      ("w", midpoint nw sw),
		      ("e", midpoint ne se)]
	val env = tree("", ptList, [])
    in (picFunc, env) end;

fun polygon ptList =
    let val picFunc = fn gc => pswrapper gc ("\\pspolygon" ^
      (handleAttrs gc) ^ (String.concat (map pointToString ptList)) ^ "\n")
	val (nw, ne, sw, se) = computeBoundingBoxFromPoints ptList
	val ptL = [("nw", nw),
		   ("ne", ne),
		   ("sw", sw),
		   ("se", se),
		   ("c", midpoint sw ne),
		   ("n", midpoint nw ne),
		   ("s", midpoint sw se),
		   ("w", midpoint nw sw),
		   ("e", midpoint ne se)]
	val env = tree("", ptL, [])
    in (picFunc, env) end;

fun openCurve ptList =
    let val picFunc = fn gc => pswrapper gc ("\\pscurve" ^
      (handleAttrs gc) ^ (String.concat (map pointToString ptList)) ^ "\n")
	val (nw, ne, sw, se) = computeBoundingBoxFromPoints ptList
	val ptL = [("nw", nw),
		   ("ne", ne),
		   ("sw", sw),
		   ("se", se),
		   ("c", midpoint sw ne),
		   ("n", midpoint nw ne),
		   ("s", midpoint sw se),
		   ("w", midpoint nw sw),
		   ("e", midpoint ne se)]
	val env = tree("", ptL, [])
    in (picFunc, env) end;
	
fun closedCurve ptList =
    let val picFunc = fn gc => pswrapper gc ("\\psccurve" ^
      (handleAttrs gc) ^ (String.concat (map pointToString ptList)) ^ "\n")
	val (nw, ne, sw, se) = computeBoundingBoxFromPoints ptList
	val ptL = [("nw", nw),
		   ("ne", ne),
		   ("sw", sw),
		   ("se", se),
		   ("c", midpoint nw se),
		   ("n", midpoint nw ne),
		   ("s", midpoint sw se),
		   ("w", midpoint nw sw),
		   ("e", midpoint ne se)]
	val env = tree("", ptL, [])
    in (picFunc, env) end;

fun wedge radius angle1 angle2 =
    let val picFunc = fn gc => pswrapper gc ("\\pswedge" ^
      (handleAttrs gc) ^ (pointToString (0.0,0.0))
	^ "{" ^ (coordToString (distance (0.0,0.0) (0.0, radius))) ^ "}"
	^ "{" ^ (coordToString angle1) ^ "}{" 
	^ (coordToString angle2) ^ "}" ^ "\n")
	val radAngle1 = (angle1 / 360.0) * 6.28318
	val radAngle2 = (angle2 / 360.0) * 6.28318
	val midAngle = radAngle1 + ((radAngle2-radAngle1)/2.0)
	val pt1 = (radius * Math.cos radAngle1,
		   radius * Math.sin radAngle1)
	val pt2 = (radius * Math.cos radAngle2,
		   radius * Math.sin radAngle2)
	val midpnt = ((radius * Math.cos midAngle), (radius*Math.sin midAngle))
	val (nw, ne, sw, se) = 
	    computeBoundingBoxFromPoints [(~radius,~radius), (radius, radius)]
	val ptList = [("start", pt1),
		      ("end", pt2),
		      ("origin", (0.0,0.0)),
		      ("nw", nw),
		      ("ne", ne),
		      ("sw", sw),
		      ("se", se),
		      ("n", midpoint nw ne),
		      ("s", midpoint sw se),
		      ("w", midpoint nw sw),
		      ("e", midpoint ne se),
		      ("c", midpnt),
		      ("midpoint", midpnt)]
		
	val env = tree("", ptList, [])
    in (picFunc, env)
    end;

fun arc radius angle1 angle2 =
    let val picFunc = fn gc => pswrapper gc ("\\psarc" ^
      (handleAttrs gc) ^ (pointToString (0.0,0.0))
	^ "{" ^ (coordToString (distance (0.0,0.0) (0.0, radius)))
	^ "}" ^ "{" ^ (coordToString angle1) ^ "}{" 
	^ (coordToString angle2) ^ "}" ^ "\n")
	val radAngle1 = (angle1 / 360.0) * 6.28318
	val radAngle2 = (angle2 / 360.0) * 6.28318
		val pt1 = (radius * Math.cos radAngle1,
		   radius * Math.sin radAngle1)
	val pt2 = (radius * Math.cos radAngle2,
		   radius * Math.sin radAngle2)
	val (nw, ne, sw, se) = 
	    computeBoundingBoxFromPoints [(0.0,0.0), pt1, pt2]
	val ptList = [("start", pt1),
		      ("end", pt2),
		      ("nw", nw),
		      ("ne", ne),
		      ("sw", sw),
		      ("se", se),
		      ("c", midpoint nw se),
		      ("n", midpoint nw ne),
		      ("s", midpoint sw se),
		      ("w", midpoint nw sw),
		      ("e", midpoint ne se)]
	val env = tree("", ptList, [])
    in (picFunc, env)
    end;

fun bezier pt1 pt2 pt3 pt4 =
    let val picFunc = fn gc => pswrapper gc ("\\psbezier" ^
        (handleAttrs gc) ^ (pointToString pt1) ^ (pointToString pt2) ^
	(pointToString pt3) ^ (pointToString pt4) ^ "\n")
	val (nw, ne, sw, se) = 
	    computeBoundingBoxFromPoints [pt1, pt2, pt3, pt4]
	val ptList = [("start", pt1),
		      ("mid1", pt2),
		      ("mid2", pt3),
		      ("end", pt4),
		      ("nw", nw),
		      ("ne", ne),
		      ("sw", sw),
		      ("se", se),
		      ("c", midpoint nw se),
		      ("n", midpoint nw ne),
		      ("s", midpoint sw se),
		      ("w", midpoint nw sw),
		      ("e", midpoint ne se)]
	val env = tree("", ptList, [])
    in (picFunc, env)
    end;


infix 6 hseq;
infix 6 vseq;
infix 6 seq;
infix 6 align;

fun lineAngle pt1 pt2 = 
    let val orig = (0.0,0.0)
	val edge = pt2--pt1
	val radius = distance orig edge
	val cosT = (fst edge)/radius
	val sinT = (snd edge)/radius
	val angle = if (req cosT 0.0) then (Math.pi/2.0) 
		    else (Math.atan (sinT/cosT))
    in if (fst edge) < 0.0 andalso (snd edge) < 0.0
	   then angle + Math.pi
       else if (fst edge) < 0.0 andalso (snd edge) > 0.0
		then angle + Math.pi
	    else angle
    end;

fun pointFromAngle pict angle = 
    let val ((tlX,tlY),(trX,trY),(blX,blY),(brX,brY)) = 
	boundingBox pict
	val newAngle = if angle < 0.0
			   then (2.0*Math.pi)+angle
		       else angle
	fun computePos c ang f = if (req (f ang) 0.0) then 0.0 else c/(f ang)
    in if ((newAngle >= 0.0) andalso (newAngle <= (Math.pi/4.0))) 
	orelse (newAngle >= (7.0*(Math.pi/4.0)))
	   then (trX, (computePos trX newAngle Math.sin)*(Math.cos newAngle))
       else if (newAngle >= (Math.pi/4.0)) andalso
	   (newAngle <= (3.0*(Math.pi/4.0)))
             then ((computePos trY newAngle Math.cos)*(Math.sin newAngle), trY)
	     else if (newAngle >= (3.0*(Math.pi/4.0))) andalso
		(newAngle <= (5.0*(Math.pi/4.0)))
              then (tlX, (computePos tlX newAngle Math.sin)*(Math.cos newAngle))
	      else ((computePos blY newAngle Math.cos)*(Math.sin newAngle), blY)
    end;


fun group (pf, tree(n, pts, pics)) = 
    (pf, tree("", pts, [(pf, tree(n,pts,pics))]));

fun (pic1 as (pf1, env1 as tree(nm1,_,_)))
    seq (pic2 as (pf2, env2 as tree(nm2,_,_))) = 
    let val pic1' = if (nm1="") then pic1 else group pic1
        val pic2' = if (nm2="") then pic2 else group pic2
    in
       let val newEnv = mergeEnvironments pic1' pic2'
	   val newFunc = fn gc => (pf1 gc) ^ "\n" ^ (pf2 gc)
       in (newFunc, newEnv)
       end
    end;

fun pic1 hseq pic2 = 
    let val (_, _, _, lr) = boundingBox pic1
	in pic1 seq (pic2 at lr)
    end;

fun (pic1,pt1) align (pic2,pt2) = 
    let val diff = pt1 -- pt2
    in pic1 seq (pic2 offsetBy diff)
    end;

fun mkseqfun ptfun1 ptfun2 =
    fn (pic1, pic2) =>
        let val pt1 = ptfun1 pic1
            val pt2 = ptfun2 pic2
        in (pic1,pt1) align (pic2,pt2)
        end;

fun pic1 vseq pic2 =
    let val (_, _, bl, _) = boundingBox pic1
        val (tl, _, _, _) = boundingBox pic2
    in (pic1, bl) align (pic2, tl)
    end;

fun tile f 1 pict = pict
  | tile f n pict = f (pict,(tile f (n-1) pict));

fun stick n pict = tile (op hseq) n pict;
fun stack n pict = tile (op vseq) n pict;
(* stuck makes no sense - it is just for testing *)
fun stuck n pict = tile (op seq) n pict;
    
fun connect pic1 pic2 lf = 
    let val c1 = center pic1
	val c2 = center pic2
	val ang1 = lineAngle c1 c2
	val ang2 = lineAngle c2 c1
	val point1 = pointFromAngle pic1 ang1
	val point2 = pointFromAngle pic2 ang2
    in (lf point1 point2)
    end;

(* Nonbasic pictures implemented in terms of other pictures *)

fun arrow pt1 pt2 = line pt1 pt2 withArrowStyle "->";
fun doublearrow pt1 pt2 = line pt1 pt2 withArrowStyle "<->";

fun hline height length = line (0.0,height) (length,height) 
    withBoundingBox ((0.0,height*2.0),(length,height*2.0),
		     (0.0,0.0),(length,0.0));

fun harrow height length = arrow (0.0,height) (length,height) 
    withBoundingBox ((0.0,height*2.0),(length,height*2.0),
		     (0.0,0.0),(length,0.0));

fun square side = box side side;
fun triangle pt1 pt2 pt3 = polygon [pt1,pt2,pt3];
fun circle radius = oval radius radius;
fun point pt1 = ((circle 0.05) at (pt1--(0.05,0.05))) withFillStyle "solid";
fun tript (x,y) = (polygon [(~0.05+x,~0.05+y),(0.05+x,~0.05+y),
				 (x,0.05+y)]) (* withFillStyle "solid"*);
fun sqpt (x,y) = (polygon [(~0.05+x,~0.05+y),(0.05+x,~0.05+y),(0.05+x,0.05+y),
			   (~0.05+x,0.05+y)]) withFillStyle "solid";
fun crpt (x,y) = (line (x,0.05+y) (x,~0.05+y)) seq (line (0.05+x,y) (~0.05+x,y));

