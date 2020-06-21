(* Text *)
(* Primitive: string -> picture *)
val thePicture = ref "";
val thePicIndex = ref [#"A",#"A"];
val boxTable = ref ([]: (string*string*string*string)list);

fun bump () =
   let val [c1,c2] = !thePicIndex
   in if (c2 = #"Z") then thePicIndex := [Char.succ c1, #"A"]
                     else thePicIndex := [c1, Char.succ c2]
   end;

fun text stuff =
  let val textnameref = ref ""
  in textnameref := !thePicture ^ (String.implode (!thePicIndex));
     let val textname = !textnameref
         val picFunc = fn (mtx,attrs) => 
         (let val a = transformPoint mtx (0.0,0.0)
            val b = transformPoint mtx (1.0,0.0)
            val c = transformPoint mtx (1.0,1.0)
            val d = transformPoint mtx (0.0,1.0)
            val theta =
              let val psi = Math.atan((snd b - snd a)/(fst b - fst a))
              in if (fst b - fst a < 0.0)
                 then psi + Math.pi
                 else psi
              end
            val thetadeg = theta * (180.0/Math.pi)
            val w = distance b a
            val h = distance d a
           in
             "\\rput[bl]{" ^ (coordToString thetadeg) ^ "}" ^
                (pointToString a) ^ "{\\scalebox{" ^
                   (coordToString w) ^ " " ^ (coordToString h) ^ "}{%\n" ^
             "\\newbox\\" ^ textname ^ "%\n"
             ^ "\\setbox\\" ^ textname ^ "=\\hbox{" ^ stuff ^ "}%\n"
             ^ "\\immediate\\write16{FPIC:" ^ textname
                 ^ ":\\the\\wd\\" ^ textname
                 ^ ":\\the\\ht\\" ^ textname
                 ^ ":\\the\\dp\\" ^ textname ^ "}%\n"
             ^ "\\box\\" ^ textname ^ "}}\n"
           end)
        val height = let val ht = getHeight textname (!boxTable)
                         val dp = getDepth textname (!boxTable)
                     in case ht of NONE => 0.25
                                 | SOME ht => case dp of
                                              NONE => ht (* shouldn't happen *)
                                            | SOME dp => ht+dp
                     end
        val width = let val wd = getWidth textname (!boxTable)
                    in case wd of NONE => 
                                   0.14 * (real (length (String.explode stuff)))
                                | SOME wd => wd
                  end
      val env = tree("", [("sw", (0.0, 0.0)),
			  ("se", (width, 0.0)),
			  ("nw", (0.0, height)),
			  ("ne", (width, height)),
			  ("c", (width/2.0, height/2.0)),
			  ("n", (width/2.0, height)),
			  ("s", (width/2.0, 0.0)),
			  ("e", (width, height/2.0)),
			  ("w", (0.0, height/2.0))], [])
    in bump ();
       (picFunc, env)
    end
  end;
