infix 6 ++;
infix 6 --;
infix 7 **;
infix 7 //;

fun (a:real,b:real) ++ (c,d) = (a+c,b+d);
fun (a:real,b:real) -- (c,d) = (a-c,b-d);
fun (a:real,b:real) ** (c,d) = (a*c,b*d);
fun (a:real,b:real) // (c,d) = (a/c, b/d);

fun distance (x1:real,y1:real) (x2,y2) = 
  Math.sqrt (((x1-x2)*(x1-x2)) + ((y1-y2)*(y1-y2)));

fun midpoint (x1:real,y1:real) (x2,y2) = 
  ((x1+x2)/2.0, (y1+y2)/2.0);
