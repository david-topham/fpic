
val fpnumber = (charseq Char.isDigit) oo (streq ".") oo
               (charseq Char.isDigit);

(* Scan log file creating a table of box sizes. *)
fun scanlog file =
  let val logfile = ref stdIn
      val boxtable = ref []
  in
     (logfile := openIn (file ^ ".log");
      scan
         (match (sol oo streq "FPIC:" oo
              (recall (charseq alphanumeric))
              oo streq ":"
              oo (recall fpnumber) oo streq "pt:"
              oo (recall fpnumber) oo streq "pt:"
              oo (recall fpnumber) oo streq "pt"))
         (fn (_,[nm,wd,ht,dp]) => (fn _ =>
           boxtable := (string nm, string wd, string ht, string dp)
                       ::(!boxtable)))
         (!logfile);
     !boxtable)
(* Moscow ML:  change the following line to "handle Io _ => []" *)
     handle Io => []
  end;

fun stringToReal s =
   case (Real.fromString s)
   of NONE => 0.0 | SOME r => r;

(* The next two functions return the height and width of a
   text box *in centimeters*.  Centimeters are the default unit
   length in pstricks, and it is the length we assume. *)

val cm_per_pt = 0.0351459803515;

fun getHeight textname [] = NONE
  | getHeight textname ((nm,wd,ht,dp)::L) =
        if textname=nm
        then SOME ((stringToReal ht) * cm_per_pt)
        else getHeight textname L;

fun getWidth textname [] = NONE
  | getWidth textname ((nm,wd,ht,dp)::L) =
        if textname=nm
        then SOME ((stringToReal wd) * cm_per_pt)
        else getWidth textname L;

fun getDepth textname [] = NONE
  | getDepth textname ((nm,wd,ht,dp)::L) =
        if textname=nm
        then SOME ((stringToReal dp) * cm_per_pt)
        else getDepth textname L;
