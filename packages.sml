use "pic.sml";

(* Pie Charts *)

exception EmptyPie;

(* Arguments to pieChart are a radius of the total chart,
   and a pieList consisting of pieslices;  a pieslice is
   a value of type percent * (radius -> angle -> picture)
*)
fun pieChart radius pieList = 
   let fun pieBuilder n [(a,pfun)] = pfun radius n
         | pieBuilder n ((a,pfun)::slices) =
	     let val newangle = n+((a/100.0) * 360.0)
             in (pfun radius n) seq (pieBuilder newangle slices)
             end
   in pieBuilder 0.0 pieList
   end;

(* slice e i p c ld returns a "pieslice", as defined above.
   e and i are strings given theexternal and internal labels
   of the slice, p is a percentage, c is a color, and ld is
   the distance of the external label from the pie.
*)
fun slice e i p c labelDist =
   let fun makeslice radius startAngle =
         let val endAngle = startAngle + ((p/100.0) * 360.0)
             val pieSlice = (wedge radius startAngle endAngle)
             val filledPie = pieSlice withFillColor c
             val degAngle = (startAngle + endAngle)/2.0
             val rotateAngle = (degAngle/360.0)*6.28318
             val xPt = (radius+labelDist)*(cos rotateAngle)
             val yPt = (radius+labelDist)*(sin rotateAngle)
             val ixPt = (radius/2.0)*(cos rotateAngle)
             val iyPt = (radius/2.0)*(sin rotateAngle)
             val extLabel = (text e) at (xPt, yPt)
             val intLabel = (text i) at (ixPt, iyPt)
         in group (namePic (filledPie seq intLabel seq extLabel) e)
         end
   in (p, makeslice)
   end;

(* Needs to be fixed to place labels correctly. *)
fun triangleSlice e i p c labelDist =
   let fun makeslice radius startAngle =
         let val endAngle = startAngle + ((p/100.0) * 360.0)
             val unitVector1 = (dcos startAngle, dsin startAngle)
             val unitVector2 = (dcos endAngle, dsin endAngle)
             val pieSlice = (triangle (0.0,0.0)
                                      (scaleVec radius unitVector1)
                                      (scaleVec radius unitVector2))
             val filledPie = pieSlice withFillColor c
             val degAngle = (startAngle + endAngle)/2.0
             val rotateAngle = (degAngle/360.0)*6.28318
             val xPt = (radius+labelDist)*(cos rotateAngle)
             val yPt = (radius+labelDist)*(sin rotateAngle)
             val ixPt = (radius/2.0)*(cos rotateAngle)
             val iyPt = (radius/2.0)*(sin rotateAngle)
             val extLabel = (text e) at (xPt, yPt)
             val intLabel = (text i) at (ixPt, iyPt)
         in group (namePic (filledPie seq intLabel seq extLabel) e)
         end
   in (p, makeslice)
   end;

fun explode (percent, picfun) =
   (percent, fn rad => (fn startAngle =>
               let val centerAngle = startAngle + ((percent/100.0) * 360.0)/2.0
               in (picfun rad startAngle)
                   offsetBy (0.5*(dcos centerAngle), 0.5*(dsin centerAngle))
               end));
   
val testChart = pieChart 5.0
    [(slice "R and D" "30 percent" 30.0 cyan 1.0),
     (slice "Marketing" "40 percent" 40.0 green 1.0),
     (slice "Others" "30 percent" 30.0 blue 1.0)];

val testChart2 = pieChart 5.0
    [(slice "R and D" "30 percent" 30.0 cyan 1.0),
     explode (slice "Marketing" "40 percent" 40.0 green 1.0),
     (slice "Others" "30 percent" 30.0 blue 1.0)];

val testChart3 = pieChart 5.0
    [(triangleSlice "R and D" "30 percent" 30.0 cyan 1.0),
     (triangleSlice "Marketing" "40 percent" 40.0 green 1.0),
     (triangleSlice "Others" "30 percent" 30.0 blue 1.0)];

val testChart4 = pieChart 5.0
    [(triangleSlice "R and D" "30 percent" 30.0 cyan 1.0),
     explode (triangleSlice "Marketing" "40 percent" 40.0 green 1.0),
     (triangleSlice "Others" "30 percent" 30.0 blue 1.0)];

draw (((snd (slice "R and D" "30 percent" 30.0 cyan 1.0)) 0.0 2.0)
    seq ((snd (slice "Marketing" "40 percent" 40.0 green 1.0)) 0.0 2.0
         at (1.0,0.0))) "test";
draw ((wedge 1.0 0.0 90.0) withFillColor red) "test";

draw testChart "pie1";
draw (explodeSlice testChart "Marketing") "pie2";
draw (testChart nthpic 2) "pie3";

(*
fun pieChart radius labelDistance pieList = 
    let fun produceSlice num (e,i,p,c) = 
	let val startAngle = num
	    val endAngle = num + ((p/100.0) * 360.0)
	    val pieSlice = (wedge radius startAngle endAngle)
	    val filledPie = pieSlice withFillColor c
	    val degAngle = startAngle + ((endAngle-startAngle)/2.0)
	    val rotateAngle = (degAngle/360.0)*6.28318
	    val xPt = (radius+labelDistance)*(cos rotateAngle)
	    val yPt = (radius+labelDistance)*(sin rotateAngle)
	    val ixPt = (radius/2.0)*(cos rotateAngle)
	    val iyPt = (radius/2.0)*(sin rotateAngle)
	    val extLabel = (text e) at (xPt, yPt)
	    val intLabel = (text i) at (ixPt, iyPt)
	in (group (namePic (filledPie seq intLabel seq extLabel) e))
	end
	fun pieBuilder n [] = raise EmptyPie
	  | pieBuilder n [(e,i,p,c)] = produceSlice n (e,i,p,c)
	  | pieBuilder n ((e,i,p,c)::r) = 
	let val offset = ((p/100.0) * 360.0)
	    in (produceSlice n (e,i,p,c)) seq 
		(pieBuilder (n+offset) r) 
	end
    in pieBuilder 0.0 pieList
    end;

fun explodeSlice piePicture sliceName = 
    let val wholePic = piePicture pic sliceName
	val actualWedge = wholePic nthpic 1
	val midpnt = actualWedge pt "midpoint"
	val orig = actualWedge pt "origin"
	val centerAngle = lineAngle orig midpnt
	val newWedge = wholePic offsetBy (0.5*(cos centerAngle),
					  0.5*(sin centerAngle))
    in replacePic piePicture newWedge sliceName
    end;
*)
    

(* Axes *)

fun axes (commonPointX, commonPointY) 
         (topLeftX, topLeftY) 
	 (bottomRightX, bottomRightY) = 
    let val xAxis = line (topLeftX, commonPointY) 
	                 (bottomRightX, commonPointY) withArrowStyle "<->"
	val yAxis = line (commonPointX, topLeftY)
                         (commonPointX, bottomRightY) withArrowStyle "<->"
        fun makeInts lo hi = if (lo > hi) then [] else lo::(makeInts (lo+1) hi)
        val xLabels = fold (fn (elt,lst)=> (line ((real elt), commonPointY-0.2) ((real elt),commonPointY+0.2)) seq
			   ((text (makestring elt)) at ((real elt), commonPointY+0.3)) seq lst)
			   (makeInts (ceiling topLeftX) (floor bottomRightX))
			   xAxis
      val yLabels = fold (fn (elt,lst)=> (line (commonPointX-0.2,(real elt)) (commonPointX+0.2,(real elt))) seq
			   ((text (makestring elt)) at (commonPointX+0.3,real elt)) seq lst)
                           (makeInts (ceiling bottomRightY) (floor topLeftY))
			   yAxis
    in xLabels seq yLabels 
    end;

fun pointPlot ptList pntFunc = 
    let val pc = 
	fold (fn (elt,lst)=> (pntFunc elt) seq lst) ptList empty
    in pc end;
    
fun linePlot [] lineFunc = empty
 |  linePlot [elt] lineFunc = empty
 |  linePlot [elt1,elt2] lineFunc = lineFunc elt1 elt2
 |  linePlot (elt1::elt2::rst) 
             lineFunc = (lineFunc elt1 elt2) seq (linePlot (elt2::rst)
							 lineFunc);

fun xyPlot ptList pntFunc lineFunc = (pointPlot ptList pntFunc) seq
                                     (linePlot ptList lineFunc);

fun genData f (loX:real) hiX ivl =
    if (loX > hiX) then []
    else (loX,(f loX))::(genData f (loX+ivl) hiX ivl);
	
val testPlot = pointPlot 
    [(~4.0,~4.0), (~3.0,~3.0),(~2.0,~2.0),(~1.0,~1.0),(0.0,0.0),(1.0,1.0),
     (2.0,2.0),(3.0,3.0),(4.0,4.0),(5.0,5.0)] point;

val sinPlot = (axes (0.0,0.0) (~5.0,5.0) (5.0,~5.0)) seq (pointPlot (genData sin ~5.0 5.0 0.02) point);

draw (axes (0.0,0.0) (~2.2,2.2) (2.2,~2.2)) "axes1";
draw (pointPlot (genData sin ~5.0 5.0 0.2) point) "xyplot";
draw (linePlot (genData sin ~5.0 5.0 0.2) line) "lineplot";
draw (xyPlot (genData sin ~5.0 5.0 0.2) crpt line) "fullplot";

(* Commutative Diagrams *)

fun buildRow l = 
    group (fold (fn (e,items)=> 
		 let val pict = fst e
		     val ((tlX,tlY),(trX,trY),bl,(brX,brY)) =
			 boundingBox pict
		     val width = trX - tlX
		     val newWidth = snd e
		     val diff = newWidth - width
		     val newBB = ((tlX,tlY),(trX+diff,trY),bl,(brX+diff,brY))
		 in ((pict withBoundingBox newBB) hseq items) end) 
	   (butlast l) (fst (last l)));
    
fun buildNodes l = 
    let val colWidths = map 
	(fn e=> let val ((tlX,_),(trX,_),_,_) = boundingBox e
		in trX - tlX
		end)
	(fold (fn (e,l) => 
	       (zip (fn (a,b)=> a vseq b) e l))
	 (butlast l) (last l))
    in fold (fn (e,row)=> row vseq (buildRow (zip (fn (a,b)=> (a,b))
					      e colWidths))) (butlast l) 
	(buildRow (zip (fn (a,b)=>(a,b)) (last l) colWidths))
    end;
	  
fun buildLines tabEnv l = 
    let fun tagOneNode r n [] = []
	  | tagOneNode r n (e::l) = (e,(r,n))::(tagOneNode r n l)
	fun tagElts r n [] = []
	  | tagElts r n (e::l) = (tagOneNode r n e)@(tagElts r (n+1) l)
	fun tagRows r [] = []
	  | tagRows r (e::l) = (tagElts r 1 e)@(tagRows (r+1) l)
	val taggedList = tagRows 1 l
    in fold (fn (((f, label, (posR, posC)),(r,c)), conn)=>
		 let val src = tabEnv nthpic r nthpic c
		     val dst = tabEnv nthpic posR nthpic posC
		     val ln = connect src dst f
		     val txt = text label at (ln pt "midpoint")
	     in (group (ln seq txt)) seq conn 
		 end) taggedList empty
    end;
	
fun comDiagram l = 
    let val tabEnv = buildNodes (map (map fst) l)
    in (group tabEnv) seq (group (buildLines tabEnv (map (map snd) l)))
    end;

val emptyCell = box 1.0 1.0 withLineColor white;

val b = buildNodes (map (map fst) 
		    [[(box 2.0 2.0 withFillColor red, [(line,"U+V",(1,4)),
                               (line,"",(4,4))]),
                   (emptyCell, []),
                   (emptyCell, []),
                   (box 2.0 2.0 withFillColor cyan, [(line, "V+X",(4,4))])],
                  [(emptyCell, []), (emptyCell, []),
                   (emptyCell, []), (emptyCell, [])],
                  [(emptyCell, []), (emptyCell, []),
                   (emptyCell, []), (emptyCell, [])],
		      [(emptyCell, []), (emptyCell, []),
                   (emptyCell, []), (box 2.0 2.0 withFillColor blue, [])]]);

fun boxes l = fold (fn (elt,lst) => 
		     (square 1.0 withFillColor elt) hseq lst) l empty;

draw (boxes [black, darkGray, lightGray, white]) "boxes";
