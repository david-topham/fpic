
val defaultsize = (2.0, 3.0);

box 1.0 2.0 hseq circle 1.5 vseq
     (label "Hello!" (oval 2.0 1.0));
draw (it scaleTo defaultsize) "paperpics/shapes";

val nose = dtriangle;
circle 2.5 seq (nose at (2.0,2.0))
           seq (nose rotate ~90.0 scale 0.7 at (1.2,2.7))
           seq (nose rotate 90.0 scale 0.7 at (3.1,2.7))
           seq doval scaleXY (0.5,0.3) at (1.7,0.7);
draw (it scaleTo defaultsize) "paperpics/pumpkin1";

val face = circle 2.5;
val facecenter = face pt "c";
val lefteye = nose rotate ~90.0 scale 0.7;
val righteye = nose rotate 90.0 scale 0.7;
val mouth = doval scaleXY (0.5,0.3);
face seq (nose centeredAt facecenter)
     seq (lefteye centeredAt (facecenter -- (1.0,~0.7)))
     seq (righteye centeredAt (facecenter ++ (1.0,0.7)))
     seq (mouth centeredAt (facecenter -- (0.0,1.5)));
draw (it scaleTo defaultsize) "paperpics/pumpkin2";

val face =
    let val f = circle 2.5
        val facecenter = f pt "c"
    in namePts f [("nosepos", facecenter),
                  ("lefteyepos", facecenter -- (0.9,~0.8)),
                  ("righteyepos", facecenter ++ (0.9,0.8)),
                  ("mouthpos", facecenter -- (0.0,1.5))]
    end;
face seq (nose centeredAt (face pt "nosepos"))
     seq (lefteye centeredAt (face pt "lefteyepos"))
     seq (righteye centeredAt (face pt "righteyepos"))
     seq (mouth centeredAt (face pt "mouthpos"));
draw (it scaleTo defaultsize) "paperpics/pumpkin3";

fun makeface f =
           let val facecenter = f pt "c"
           in namePts f [("nosepos", facecenter),
                         ("lefteyepos", facecenter -- (0.9,~0.8)),
                         ("righteyepos", facecenter ++ (0.9,0.8)),
                         ("mouthpos", facecenter -- (0.0,1.5))]
           end;
(makeface (box 5.0 5.0))
     seq (placePt nose "c" (face pt "nosepos"))
     seq (placePt lefteye "c" (face pt "lefteyepos"))
     seq (placePt righteye "c" (face pt "righteyepos"))
     seq (placePt mouth "c" (face pt "mouthpos"));
draw (it scaleTo defaultsize) "paperpics/pumpkin4";

val cell = let val car = namePic dbox "car"
               val cdr = namePic dbox "cdr"
           in car vseq cdr
           end;
draw (cell scaleTo defaultsize) "paperpics/cellv1";

val cell = let val c = stack 2 dbox
           in addNamedPics c [("car", c nthpic 1),
                              ("cdr", c nthpic 2)]
           end;
draw (cell scaleTo defaultsize) "paperpics/cellv2";

val cells =
let val cells = (namePic cell "left") hseq (hspace 1.0) hseq 
                (namePic cell "right")
    val source = cells pic "left" pic "cdr" pt "c"
    val target = cells pic "right" pic "car" pt "w"
in cells seq (bezier source (source ++ (1.0,0.0))
                     (target -- (1.0,0.0)) target
              withArrowStyle "->")
end;
draw (cells scaleTo defaultsize) "paperpics/twocells";

fun sierpinski 0 = dtriangle 
  | sierpinski n =
       let val s = sierpinski (n-1) scaleWithPoint
                          ((0.5,0.5),(0.0,0.0))
       in s hseq s seq (s at ((width s)/2.0, height s))
       end;
draw ((sierpinski 3) scaleTo defaultsize) "paperpics/sierp";

fun hseqtopsp gap = mkseqfun (fn p => northeast (p right 1.0)) northwest;
fun hseqtopsplist gap = mkseqlist (hseqtopsp gap);

fun drawtree root subtrees =
   let val bottom = hseqtopsplist 1.0 subtrees
       val top = placePt root "s" (bottom pt "n" ++ (0.0,1.0))
       val rootsouth = top pt "s"
   in group (top seq bottom seq
       (seqlist (map (fn p => line rootsouth (p pt "n")) (pics bottom))))
   end;

let val t = drawtree dcircle [dbox, dbox]
  in drawtree dbox [t, t] end;
draw (it scaleTo defaultsize) "paperpics/tree1";

fun drawtree root subtrees linefun =
   let val bottom = hseqtopsplist 1.0 subtrees
       val top = placePt root "s" (bottom pt "n" ++ (0.0,1.0))
       val rootsouth = top pt "s"
   in group (top seq bottom seq
       (seqlist (map (fn p => linefun rootsouth (p pt "n")) (pics bottom))))
   end;

let val t = drawtree dcircle [dbox, dbox] line
  in drawtree dbox [t, t] line end;
draw (it scaleTo defaultsize) "paperpics/tree2";

let val t = drawtree dcircle [dbox, dbox] arrow
  in drawtree dbox [t, t] arrow end;
draw (it scaleTo defaultsize) "paperpics/tree3";

fun curvedvline pt1 pt2 =
   bezier pt1 (pt1--(0.0,1.0)) (pt2++(0.0,1.0)) pt2;

let val t = drawtree dcircle [dbox, dbox] curvedvline
  in drawtree dbox [t, t] curvedvline end;
draw (it scaleTo defaultsize) "paperpics/tree4";

fun manline pt1 pt2 =
   let val ymid = (snd pt1 + snd pt2)/2.0
   in seqlist [line pt1 (fst pt1, ymid),
               line (fst pt1, ymid) (fst pt2, ymid),
               line (fst pt2, ymid) pt2]
   end;
let val t = drawtree dcircle [dbox, dbox] manline
  in drawtree dbox [t, t] manline end;
draw (it scaleTo defaultsize) "paperpics/tree5";

fun shortline linefun pt1 pt2 =
    let val diff = pt2 -- pt1;
        val pt1' = pt1 ++ diff**(0.25,0.25);
        val pt2' = pt2 -- diff**(0.25,0.25)
    in linefun pt1' pt2'
    end;

let val t = drawtree dcircle [dbox, dbox]
                            (shortline arrow)
  in drawtree dbox [t, t] (shortline arrow) end;
draw (it scaleTo defaultsize) "paperpics/tree6";

let val t = drawtree dcircle [dbox, dbox]
                            (shortline manline)
  in drawtree dbox [t, t] (shortline manline) end;
draw (it scaleTo defaultsize) "paperpics/tree7";

infix 6 cseq;
fun (p1 cseq p2) = (p1, center p1) align (p2, center p2);
val cseqlist = mkseqlist (op cseq);

val bullseye = cseqlist (map
       (fn rad => circle rad withFillColor
                         (1.0/rad, 1.0/rad, 1.0/rad))
      [5.0, 4.0, 3.0, 2.0, 1.0]);
draw (bullseye scaleTo defaultsize) "paperpics/bullseye";

infix 7 cellseq;
fun cell1 cellseq cell2 =
   let val cells = (group cell1) hseq (hspace 1.0) hseq (group cell2)
   in cells seq curvedharrow (cells nthpic 1 pic "cdr" pt "c")
                             (cells nthpic 3 pic "car" pt "w")
   end;
val cellseqlist = mkseqlist (op cellseq);
fun labelCar L =
   cell seq (L centeredAt (cell pic "car" pt "c"));
cellseqlist
 (map labelCar
           [text "A",
            dcircle scaleTo (height (cell pic "car"),
                             width (cell pic "car")),
            cell scale 0.3]);
draw (it scaleTo (scaleVec 1.3 defaultsize)) "paperpics/cellseq";

fun pieChart radius pieList = 
   let fun pieBuilder n [(a,pfun)] = pfun radius n
         | pieBuilder n ((a,pfun)::slices) =
	     let val newangle = n+((a/100.0) * 360.0)
             in (pfun radius n) seq (pieBuilder newangle slices)
             end
   in pieBuilder 0.0 pieList
   end;

fun slice lab percent color =
   let fun makeslice radius startAngle =
         let val endAngle = startAngle + ((percent/100.0) * 360.0)
             val pieSlice = (wedge radius startAngle endAngle)
             val filledPie = pieSlice withFillColor color
             val degAngle = (startAngle + endAngle)/2.0
             val rotateAngle = (degAngle/360.0)*6.28318
             val labelDist = radius/5.0
             val xPt = (radius+labelDist)*(cos rotateAngle)
             val yPt = (radius+labelDist)*(sin rotateAngle)
             val extLabel = (text lab) centeredAt (xPt, yPt)
         in filledPie seq extLabel
         end
   in (percent, makeslice)
   end;

fun triangleSlice lab percent color =
   let fun makeslice radius startAngle =
         let val endAngle = startAngle + ((percent/100.0) * 360.0)
             val unitVec1 = (dcos startAngle, dsin startAngle)
             val unitVec2 = (dcos endAngle, dsin endAngle)
             val pieSlice = (triangle (0.0,0.0)
                                      (scaleVec radius unitVec1)
                                      (scaleVec radius unitVec2))
             val filledPie = pieSlice withFillColor color
             val midAngle = (startAngle + endAngle)/2.0
             val unitVec3 = (dcos midAngle, dsin midAngle)
             val bisect = midpoint (scaleVec radius unitVec1)
                                   (scaleVec radius unitVec2)
             val labelLoc = bisect ++ (scaleVec (radius/5.0) unitVec3)
             val extLabel = (text lab) centeredAt labelLoc
         in filledPie seq extLabel
         end
   in (percent, makeslice)
   end;

fun explodeSlice (percent, picfun) =
   (percent, fn rad => (fn startAngle =>
               let val angleDelta = ((percent/100.0) * 360.0)/2.0
                   val centerAngle = startAngle + angleDelta
                   val centerUnitVec = (dcos centerAngle, dsin centerAngle)
               in (picfun rad startAngle)
                   offsetBy (scaleVec 1.0 centerUnitVec)
               end));
   
val testChart1 =
  pieChart 5.0
    [(slice "Northeast" 20.0 cyan),
     (slice "Southeast" 25.0 green),
     (slice "Midwest" 30.0 blue),
     (slice "Southwest" 10.0 red),
     (slice "Northwest" 15.0 yellow)];
draw (testChart1 scaleTo (5.0,5.0)) "paperpics/pie1";

val testChart2 =
  pieChart 5.0
    [(slice "Northeast" 20.0 cyan),
     explodeSlice (slice "Southeast" 25.0 green),
     (slice "Midwest" 30.0 blue),
     explodeSlice (slice "Southwest" 10.0 red),
     (slice "Northwest" 15.0 yellow)];
draw (testChart2 scaleTo (5.0,5.0)) "paperpics/pie2";

val testChart3 =
  pieChart 5.0
    [(triangleSlice "Northeast" 20.0 cyan),
     (triangleSlice "Southeast" 25.0 green),
     (triangleSlice "Midwest" 30.0 blue),
     (triangleSlice "Southwest" 10.0 red),
     (triangleSlice "Northwest" 15.0 yellow)];
draw (testChart3 scaleTo (5.0,5.0)) "paperpics/pie3";

val testChart4 =
  pieChart 5.0
    [(triangleSlice "Northeast" 20.0 cyan),
     explodeSlice (triangleSlice "Southeast" 25.0 green),
     (triangleSlice "Midwest" 30.0 blue),
     explodeSlice (triangleSlice "Southwest" 10.0 red),
     (triangleSlice "Northwest" 15.0 yellow)];
draw (testChart4 scaleTo (5.0,5.0)) "paperpics/pie4";

