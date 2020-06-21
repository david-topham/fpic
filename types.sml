type Point = real*real;
type Vector = real*real;

type Color = real * real * real;
type Matrix = (real*real*real)*(real*real*real)
type GraphicsContext = Matrix * (string*string) list 
type BoundingBox = Point * Point * Point * Point;

type NamedPoint = string * Point;

datatype Environment = tree of string * NamedPoint list *
         ((GraphicsContext->string) * Environment) list;

type BitMap = string;

type Picture = (GraphicsContext->BitMap) * Environment;
