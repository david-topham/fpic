boxTable := scanlog "tryit";
thePicture := "pix";
thePicIndex := [#"A",#"A"];

val b1 = box 2.0 4.0;
b1 hseq (b1 scaleXY (0.5,0.5));
draw it "fpic-pix";
thePicture := "list";
thePicIndex := [#"A",#"A"];

val cell = let val car = namePic dbox "car"
               val cdr = namePic dbox "cdr"
           in car vseq cdr end;

let val cells = (namePic cell "left") hseq (hspace 1.0) hseq
                (namePic cell "right")
    val source = cells pic "left" pic "cdr" pt "c"
    val target = cells pic "right" pic "car" pt "w"
in cells seq (bezier source (source ++ (1.0,0.0))
                     (target -- (1.0,0.0)) target
              withArrowStyle "->")
end;
draw it "fpic-list";
