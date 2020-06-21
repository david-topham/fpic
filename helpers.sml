exception ListError;

fun last [] = raise ListError
  | last [l] = l
  | last (e::l) = last l;

fun butlast [] = []
  | butlast [l] = []
  | butlast (e::l) = e::(butlast l);

fun zip f l1 [] = []
  | zip f [] l2 = []
  | zip f (e1::l1) (e2::l2) = (f (e1,e2))::(zip f l1 l2);

fun compose f g x = f (g x);

fun coordToString (x:real) =
    let val tx = (real (trunc (x*1000000.0)))/1000000.0
    in if (abs x) < 0.0001 then "0.0"
        else if x < 0.0 then "-"^(Real.toString (abs tx))
                        else (Real.toString tx)
    end;

fun pointToString (x:real,y:real) =
    "(" ^ (coordToString x) ^ ", " ^ (coordToString y) ^ ")";

fun pairToString (x:real,y:real) =
    (coordToString x) ^ " " ^ (coordToString y);

fun colorToString (x:real,y:real,z:real) =
  (* Pstricks cannot handle long numbers in rgb specs (a bug, I suppose),
     so we need to reduce their size. *)
  (coordToString x) ^ " " ^ (coordToString y) ^ " " ^ (coordToString z);

fun minR (x:real,y) = if x < y then x else y;
fun maxR (x:real,y) = if x < y then y else x;

fun minRList [] = 0.0 (* Completeness only *)
 |   minRList (x::nil) = x
 |   minRList (e::l) = let val result = minRList l
		       in minR(e, result)
		       end;

fun maxRList [] = 0.0 (* Completeness only *)
 |   maxRList (x::nil) = x
 |   maxRList (e::l) = let val result = maxRList l
		       in maxR(e, result)
		       end;

fun dcos x = Math.cos (x*Math.pi/180.0);
fun dsin x = Math.sin (x*Math.pi/180.0);
fun scaleVec (s:real) (x,y) = (s*x, s*y);
fun req x y = (abs (x-y)) < 0.00001;
