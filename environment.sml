exception ItemNotFound;

infix 8 pt;
infix 8 pic;
infix 8 nthpic;

fun simple (_, tree(_,_,[])) = true
  | simple _ = false;

fun composite pict = not (simple pict);

fun ((_ , tree(_, points, pics)) pt item) =
  let fun piclis [] item = raise ItemNotFound
        | piclis (pic1::pics) item =
             (pic1 pt item)
               handle ItemNotFound => piclis pics item;
      fun ptlis [] item = raise ItemNotFound
        | ptlis ((s,p)::r) item =
             if item = s then p else ptlis r item
  in (ptlis points item) handle ItemNotFound => piclis pics item
  end;

fun (thispic as (_, tree(nm, _, pics))) pic item =
  let fun piclis [] item = raise ItemNotFound
        | piclis (pic1::pics) item =
             (pic1 pic item)
               handle ItemNotFound => piclis pics item;
  in if (nm = item) then thispic else piclis pics item
  end;

fun (_, tree(_, _, [])) nthpic _ = raise ItemNotFound
  | (d1, tree(d2, d3, e::l)) nthpic n =
    if n = 1 then e else (d1, tree(d2, d3, l)) nthpic (n-1);

fun namePt (d1, tree(d2, l, d3)) name pnt = 
    (d1, tree(d2, (name,pnt)::l, d3));

fun namePts aPicture [] = aPicture
  | namePts aPicture ((name, pnt)::r) = 
        namePts (namePt aPicture name pnt) r;

fun namePic (d1, tree(_, d2, d3)) name =
    (d1, tree(name, d2, d3));

fun addNamedPic (pf1, tree(n1, pts1, pics1)) name
            (pic2 as (pf2, tree(n2, pts2, pics2))) =
    (pf1, tree(n1, pts1, (pf2, tree(name, [], [pic2]))::pics1));

fun addNamedPics aPicture [] = aPicture
  | addNamedPics aPicture ((name, p)::r) = 
        addNamedPics (addNamedPic aPicture name p) r;
    
fun pics (_, tree(_,_,sp)) = sp;
fun pts (_, tree(_,p,_)) = p;

fun removeHelper [] name = [] 
  | removeHelper ((d1,tree(d2,d3,sp))::r) name =
    if (name = d2) then r
    else (d1,tree(d2,d3,(removeHelper sp name)))::(removeHelper r name);

fun replaceHelper [] newPic name = []
  | replaceHelper ((d1,tree(d2,d3,sp))::r) newPic name = 
    if (name = d2) then (newPic::r)
    else ((d1,tree(d2,d3,(replaceHelper sp newPic name)))::
	  (replaceHelper r newPic name));

fun replaceNthHelper [] newPic oldNum newNum = []
  | replaceNthHelper (e::r) newPic oldNum newNum = 
    if (oldNum = newNum) then (newPic::r)
    else e::(replaceNthHelper r newPic (oldNum+1) newNum);

fun removePic (d1,tree(d2,d3,d4)) name = 
    (d1,tree(d2,d3,removeHelper d4 name));

fun replacePic (d1,tree(d2,d3,d4)) newPic name =
    (d1,tree(d2,d3,replaceHelper d4 newPic name));

fun replaceNthPic (d1,tree(d2,d3,d4)) newPic num = 
    (d1,tree(d2,d3,replaceNthHelper d4 newPic 1 num));

(* Determines the bounding box of a picture based on the current
position of the original corner points *)
fun boundingBox pict = 
    let val (neX,neY) = pict pt "ne"
	val (seX,seY) = pict pt "se"
	val (nwX,nwY) = pict pt "nw"
	val (swX,swY) = pict pt "sw"
	val minX = minRList [neX, seX, nwX, swX]
	val minY = minRList [neY, seY, nwY, swY]
	val maxX = maxRList [neX, seX, nwX, swX]
	val maxY = maxRList [neY, seY, nwY, swY]
    in ((minX, maxY), (maxX, maxY), (minX, minY), (maxX, minY))
    end;

fun width pict = let val (tl,tr,_,_) = boundingBox pict
		     val (width,_) = tr--tl
		 in width end;

fun height pict = let val (tl,_,bl,_) = boundingBox pict
		      val (_,height) = tl--bl
		  in height end;

(* The following functions calculate the true compass points, as
   given by the bounding box.  The named points may have been
   recalculated as a result of transformations. *)
fun center pict = let val (tl, tr, bl, br) = boundingBox pict
		  in midpoint tl br
		  end;

fun north pict = let val (tl, tr, bl, br) = boundingBox pict
	         in midpoint tl tr
	         end;

fun south pict = let val (tl, tr, bl, br) = boundingBox pict
	         in midpoint bl br
	         end;

fun east pict = let val (tl, tr, bl, br) = boundingBox pict
	         in midpoint tr br
	         end;

fun west pict = let val (tl, tr, bl, br) = boundingBox pict
	         in midpoint tl bl
	         end;

fun northeast pict = let val (tl, tr, bl, br) = boundingBox pict
	         in tr
	         end;

fun northwest pict = let val (tl, tr, bl, br) = boundingBox pict
	         in tl
	         end;

fun southeast pict = let val (tl, tr, bl, br) = boundingBox pict
	         in br
	         end;

fun southwest pict = let val (tl, tr, bl, br) = boundingBox pict
	         in bl
	         end;

(* Determines the bounding box of a line *)
fun fst (x,_) = x;
fun snd (_,y) = y;
fun computeBoundingBoxFromPoints ptList = 
    let val xList = map fst ptList
	val yList = map snd ptList
	val minX = minRList xList
	val minY = minRList yList
	val maxX = maxRList xList 
	val maxY = maxRList yList
    in ((minX, maxY), (maxX, maxY), (minX, minY), (maxX, minY))
    end;

(* The new bounding box that results when two pictures are 
 combined.  Used to determine corner points of new picture. *)
fun mergeBoundingBoxes (x1,x2,x3,x4) (y1,y2,y3,y4) = 
    computeBoundingBoxFromPoints [x1,x2,x3,x4,y1,y2,y3,y4]
    
(* The new environment that results when two pictures are
 combined *)
fun mergeEnvironments (pf1, tree(n1, pts1, pics1))
                      (pf2, tree(n2, pts2, pics2)) = 
    let val pic1 = (pf1, tree(n1, pts1, pics1))
	val pic2 = (pf2, tree(n2, pts2, pics2))
	val (nw, ne, sw, se) = 
	    mergeBoundingBoxes (boundingBox pic1) (boundingBox pic2)
	val newPts = [("nw", nw),
		      ("ne", ne),
		      ("sw", sw),
		      ("se", se),
		      ("c", midpoint nw se),
		      ("n", midpoint nw ne),
		      ("s", midpoint sw se),
		      ("w", midpoint nw sw),
		      ("e", midpoint ne se)]
    in if null pics1 
	then if null pics2
	       then tree("", newPts, [pic1, pic2])
	     else tree("", newPts, pic1::pics2)
        else if null pics2
	       then tree("", newPts, pics1@[pic2])
	     else tree("", newPts, pics1@pics2)
    end;

