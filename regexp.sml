
open Substring;

(* sscat concatenates ss1 and ss2, under the assumption that
   they are contiguous substrings of the same string *)
fun sscat ss1 ss2 =
   let val (s,i,n) = base ss1
   in substring(s,i,size ss1 + size ss2)
   end;
  
type RESuccess = (substring * substring) * (substring list);
type REOutcome = RESuccess option;
type RegExp = substring -> REOutcome;

fun streq str =
  fn ss => if (isPrefix str ss)
            then SOME (splitAt(ss, String.size str), []:substring list)
            else NONE;

fun charseq pred =
  fn ss => let fun pref i = (if pred (sub(ss,i))
                             then pref (i+1)
                             else i)
                            handle Subscript => i
           in let val p = pref 0
                  val s =splitAt(ss, pref 0)
              in SOME (s, []:substring list)
              end
           end;

fun alphanumeric c =
  (Char.isAlphaNum c) orelse (Char.contains "_" c);

fun nonempty R = (fn res => case res of
                    NONE => NONE
                  | SOME((h,t),L) => if size h = 0
                                     then NONE
                                     else res)
               o R;

fun recall R = (fn NONE => NONE
                 | SOME ((h,t),L) => SOME ((h,t), h::L))
             o R;

fun sol ss = let val (s,i,n) = base ss
             in if i=0 then SOME(splitAt(ss,0), []:substring list)
                       else NONE
             end;

fun eol ss = let val (s,i,_) = base ss
             in if i=String.size s
                then SOME(splitAt(ss,0), []:substring list)
                else NONE
             end;

infix 2 oo;
fun (R1 oo R2) ss =
   case R1 ss of NONE => NONE
               | SOME ((h,t),L) =>
                    case R2 t of NONE => NONE
                               | SOME ((h',t'), L') =>
                                     SOME ((sscat h h', t'), L@L');

(* Various ways to use reg. expr.'s *)
fun match R str = R (full str);
fun matchFirst R str = (* find first match, not necessarily at the
  start of str *) R (full str);  (* not implemented *)
fun matchAll R str = (* find all matches, not necessarily at the
  start of str *) R (full str);  (* not implemented *)

open TextIO;
fun scan
     (pred:string->REOutcome)        (* predicate built from reg. exp. *)
     (f:RESuccess -> string -> unit) (* function to process each line *)
     (is:instream)                   (* input *)
  = if endOfStream is
    then ()
    else (let val ln = inputLine is
              val m = pred (case ln of SOME ln' => ln')
          in case m of
             NONE => ()
           | SOME info => f info (case ln of SOME ln' => ln')
          end;
          scan pred f is);

fun scanUpto
     (pred:string->REOutcome)  (* terminating predicate *)
     (f: string -> unit)       (* function to process each line *)
     (is:instream)             (* input *)
  = if endOfStream is
    then ()
    else let val ln = inputLine is
             val m = pred (case ln of SOME ln' => ln')
         in case m of
            NONE => (f (case ln of SOME ln' => ln'); scanUpto pred f is)
          | SOME info => ()
         end;

fun processTeXfile TeXfilename = (* give name without ".tex" *)
  let val sourcefile = openIn (TeXfilename ^ ".tex")
      val fpicfile = openOut "fpicpics.sml"
  in
      output(fpicfile, "boxTable := scanlog \"" ^ TeXfilename ^ "\";\n");
      scan
        (match (streq "\\fpic{" oo
                   (recall (nonempty (charseq alphanumeric))) oo
                   streq "}{"))
        (fn ((_,t),L) => (fn ln =>
             (let val picname = (string (hd L))
                  val picfilename = "fpic-" ^ picname
              in
                 output(fpicfile, "thePicture := \"" ^ picname ^ "\";\n");
                 output(fpicfile, "thePicIndex := [#\"A\",#\"A\"];\n");
                 output(fpicfile, string t);  (* remainder of this line *)
                 scanUpto
                     (match (sol oo streq "}"))
                     (fn ln => output(fpicfile, ln))
                     sourcefile;
                 output(fpicfile, "draw it \"" ^ picfilename ^ "\";\n")
              end)))
        sourcefile;
     closeIn sourcefile;
     closeOut fpicfile
end;

(* In above, if we choose to go with environment-style braces,
   change the two reg. expr.'s to:

        (match (sol oo streq "\\begin{fpic}{" oo
                   (recall (nonempty (charseq alphanumeric))) oo streq "}"))

                     (match (sol oo streq "\\end{fpic}"))
*)
