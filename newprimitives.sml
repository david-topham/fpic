(* place and placePt are for placing one picture such that one
   of its points coincides with some point.

   "place p1 ptfun pnt" places p1 such that (ptfun p1) coincides with pnt.
   "placePt p1 nm pnt" place p1 such that (p1 pt nm) coincides with ptn.  *)
fun trueOrigin p =
    let val (_,_,sw,_) = boundingBox p in sw end;
fun place p1 ptfun pnt =
    p1 at ((trueOrigin p1) ++ pnt -- (ptfun p1)) ;
fun placePt p1 name = place p1 (fn p => p pt name) ;

(* Just a chunk of real estate, used to create space in which
   to center stuff *)
fun clearbox w h = empty withBoundingBox
                 ((0.0,h), (w,h), (0.0,0.0), (w,0.0));

(* Spaces for vertical and horizontal spacing *)
fun hspace w = clearbox w 0.0;
fun vspace h = clearbox 0.0 h;

(* Sequencing operations are binary, but should be extended to
   operate on lists;  this is a lot like foldr *)
fun mkseqlist seqop [] = empty
  | mkseqlist seqop [p:Picture] = p
  | mkseqlist seqop (p::ps) = seqop (p, mkseqlist seqop ps);

val seqlist = mkseqlist (op seq);
val hseqlist = mkseqlist (op hseq);
val vseqlist = mkseqlist (op vseq);

(* Sequencing by center pictures over one another *)
infix 6 cseq;
fun (p1 cseq p2) = (p1, center p1) align (p2, center p2);
val cseqlist = mkseqlist (op cseq);

(* Horizontal and vertical sequencing with additional space
   added.  (Syntax is kind of crufty here, unfortunately.) *)
infix 6 hseqsp;
fun (p1 hseqsp gap) p2 =
    p1 hseq (p2 offsetBy (gap,0.0));
fun hseqsplist gap = mkseqlist (fn (p1, p2) => (p1 hseqsp gap) p2);

infix 6 vseqsp;
fun (p1 vseqsp gap) p2 =
    let val (_,_,bl,_) = boundingBox p1
    in p1 seq (p2 at (bl ++ (0.0, gap)))
    end;
fun vseqsplist gap = mkseqlist (fn (p1, p2) => (p1 vseqsp gap) p2);

(* Sequencing appropriate for lines - place "start" of p2 at
   "end" of p1 *)
infix 6 lineseq;
fun (p1 lineseq p2) =
   let val p2' = placePt p2 "start" (p1 pt "end")
   in namePts (p1 seq p2')
        [("start", (p1 pt "start")), ("end", (p2' pt "end"))]
   end;
val lineseqlist = mkseqlist (op lineseq);

(* Center text within a picture. *)
fun label txt p = p cseq (text txt);

(* curvedhline and curvedvline differ in how the initial
   segment of the line is draw (either horizontally or
   vertically).  Should have a more general curved line-drawing
   function that takes two angles, the angle of exit from the
   source and of entrance into the target. *)
fun curvedhline pt1 pt2 =
   let val xdist = (fst pt2 - fst pt1);
       val ydist = (snd pt2 - snd pt1);
       val pt3 = pt1++(xdist/3.0,ydist/10.0);
       val pt4 = pt2--(xdist/3.0,ydist/10.0)
   in openCurve [pt1,pt3,pt4,pt2]
   end;
fun curvedharrow pt1 pt2 = curvedhline pt1 pt2 withArrowStyle "->";

fun curvedvline pt1 pt2 =
   let val xdist = (fst pt2 - fst pt1);
       val ydist = (snd pt2 - snd pt1);
       val pt3 = pt1++(xdist/10.0,ydist/3.0);
       val pt4 = pt2--(xdist/10.0,ydist/3.0)
   in openCurve [pt1,pt3,pt4,pt2]
   end;
fun curvedvarrow pt1 pt2 = curvedvline pt1 pt2 withArrowStyle "->";

(* arccw is the same as arc except that "start" and "end" points
   are reversed. *)
fun arccw r angle1 angle2 =
             let val thearc = arc r angle1 angle2
             in namePts thearc
                 [("start", (thearc pt "end")),
                  ("end", (thearc pt "start"))]
             end;

(* scaleTo scaled picture p so that either its height is h or
   its width is w, but neither exceeds its respective number. *)
infix 3 scaleTo;
fun (p scaleTo (w:real, h:real)) =
   let val hscale = h/(height p)
       val wscale = w/(width p)
       val s = if (hscale < wscale) then hscale else wscale
   in p scale s
   end;

(* resize scales picture p so that its height is h and its
   width is w *)
infix 3 resize;
fun (p resize (w:real, h:real)) =
   let val hscale = h/(height p)
       val wscale = w/(width p)
   in p scaleXY (wscale, hscale)
   end;

val dbox = box 1.618034 1.0;  (* Golden ratio *)
val doval = oval 1.618034 1.0;
val dcircle = circle 1.0;
val dtriangle = triangle (0.0,0.0) (1.0,0.0) (0.5,0.8660254);

infix 6 up;
fun (p up dy) = p offsetBy (0.0,dy);
infix 6 down;
fun (p down dy) = p offsetBy (0.0,~dy);
infix 6 left;
fun (p left dx) = p offsetBy (~dx,0.0);
infix 6 right;
fun (p right dx) = p offsetBy (dx,0.0);
