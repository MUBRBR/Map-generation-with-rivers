#r "makeBMP.dll"
open System
open System.Collections.Generic

let mutable rivers = 1 // indicates that rivers are made

let width = 1023 // size of bitmap array

let heights = Array2D.create width width -2.0
let river = Array2D.create width width false

// k1 and k2 defined in rivers-papers..
let k1 = 0.32
let k2 = 0.55

let min3 x y z = min x (min y z)
let max3 x y z = max x (max y z)

// magic constant for PRNG
let magic = 100.0 * System.Math.PI

// make new seed from two seeds
let mix (seed1 : float) (seed2 : float) =
  let a : float = (seed1 + magic)
  let b : float = (seed2 + magic)
  (a*b) % 2.0 - 1.0

// make new seed from one seed
let nxt (seed : float) =
  let a : float = (seed + magic)
  (a*a) % 2.0 - 1.0
  
// random value between a and b, tend towards middle
let between(a,b,seed) =
  (a + b + seed*seed*seed*(a-b))/2.0

type Vert = { x: float; y: float; s: float; h: float }
type Triangle = {vert0 : Vert; vert1 : Vert; vert2 : Vert; e0 : float; e1 : float; e2 : float}

type Position = {x:float; y:float}

// distance between points
let distance (p0:Vert) (p1:Vert) =
  let dx: float = p1.x-p0.x
  let dy: float = p1.y-p0.y
  sqrt(dx*dx+dy*dy)


//  v0-----e2-----v1
//   |\          /
//   | \        /
//   |  \      e5
//   |  e3    /
//   |    \  /
//  e1     \/
//   |     / v3
//   |    /    e0 = e4++e5
//   |   e4
//   |  /
//   | /
//   v2

let outsideBounds (triangle : Triangle, xmin: float, xmax: float, ymin: float, ymax: float) : bool =
    triangle.vert0.x < xmin && triangle.vert1.x < xmin && triangle.vert2.x < xmin ||
    triangle.vert0.x > xmax && triangle.vert1.x > xmax && triangle.vert2.x > xmax ||
    triangle.vert0.y < ymin && triangle.vert1.y < ymin && triangle.vert2.y < ymin ||
    triangle.vert0.y > ymax && triangle.vert1.y > ymax && triangle.vert2.y > ymax

let extend(v: Vert, e: float, vNear: Vert, vFar: Vert, s: float) =
  if  v.h < min 0.0 e && 0.0 < vFar.h then
    between(e, v.h, s) // extend down
  else if abs s < 0.65 && e < min3 v.h vNear.h vFar.h then
    between(e, min3 v.h vNear.h vFar.h, nxt v.s) // extend up
  else -100.0 // don't extend

// Calculating area of triangle by points only
// https://www.cuemath.com/geometry/area-of-triangle-in-coordinate-geometry/
let area (x0:float, y0:float, x1:float, y1:float, x2:float, y2:float) : float =
  abs((x0*(y1-y2) + x1*(y2-y0) + x2*(y0-y1)) / 2.0)

// Find out if a point is inside a triangle
// https://www.geeksforgeeks.org/check-whether-a-given-point-lies-inside-a-triangle-or-not/
let isInside (point : Position) (triangle : Triangle) : bool =
  // Find total area of triangle.
  // Then find area of the 3 triangles created from our point to the vertices.
  let totalArea : float = area(triangle.vert0.x, triangle.vert0.y, triangle.vert1.x, triangle.vert1.y, triangle.vert2.x, triangle.vert2.y)
  let areaP01 : float = area(point.x, point.y, triangle.vert0.x, triangle.vert0.y, triangle.vert1.x, triangle.vert1.y)
  let areaP12 : float = area(point.x, point.y, triangle.vert1.x, triangle.vert1.y, triangle.vert2.x, triangle.vert2.y)
  let areaP02 : float = area(point.x, point.y, triangle.vert0.x, triangle.vert0.y, triangle.vert2.x, triangle.vert2.y)
  // Using a small epsilon for tolerance since we are dealing with floats
  let epsilon : float = 0.0000000001
  abs(totalArea - (areaP01+areaP12+areaP02)) < epsilon

let triaLoop (v0:Vert) (v1:Vert) (v2:Vert) (e0:float) (e1:float) (e2:float) (xmin:float) (xmax:float) (ymin:float) (ymax:float) (epsilon:float) =
  for i = 0 to width-1 do
    for j = 0 to width-1 do
      let currentPosition: Position = {
          x=xmin + (float i + 0.5) * epsilon;
          y=ymin + (float j + 0.5) * epsilon}
      let mutable curTri: Triangle = {
                  vert0=v0; vert1=v1; vert2=v2; 
                  e0=e0; e1=e1; e2=e2}
      if not (isInside currentPosition curTri) then ()
      // For each pixel/position, iterate through subtriangles untill finding out which one it belongs to.
      else
        while distance curTri.vert1 curTri.vert2 >= epsilon do
          let dist: float = distance curTri.vert1 curTri.vert2
          let delta: float = k1*dist + k2*abs(curTri.vert1.h - curTri.vert2.h)
          let s3: float = mix curTri.vert1.s curTri.vert2.s
          let x3: float = (curTri.vert1.x + curTri.vert2.x) / 2.0
          let y3: float = (curTri.vert1.y + curTri.vert2.y) / 2.0
          let (e4:float, e5: float, h3: float) =
            match curTri.e0 with
            | (h: float) when h > -100 ->  
                    if abs(h - curTri.vert1.h) > abs(h - curTri.vert2.h)
                    then curTri.e0, -100, (curTri.e0+curTri.vert1.h)/2.0+s3*delta
                    else -100, curTri.e0, (curTri.e0+curTri.vert2.h)/2.0+s3*delta
            | _ -> 
                    -100, -100, (curTri.vert1.h + curTri.vert2.h)/2.0 + s3*delta
          
          let v3: Vert = {x=x3; y=y3; s=s3; h=h3}
          let s03 : float = mix curTri.vert0.s s3
          let e1Active : bool = curTri.e1 > -100.0
          let e2Active : bool = curTri.e2 > -100.0
          let e4Active : bool = e4 > -100.0
          let e5Active : bool = e5 > -100.0
          
          let e3 : float =
            match e1Active, e2Active, e4Active, e5Active with
            | false, false, false, false
                ->  
                    if rivers = 1 && max curTri.vert1.h curTri.vert2.h > 0.1 
                      && min curTri.vert1.h curTri.vert2.h < min3 curTri.vert0.h v3.h -0.1
                    then between(min curTri.vert1.h curTri.vert2.h, min curTri.vert0.h v3.h, s03)
                    else -100.0
            | true, false, false, false -> extend(curTri.vert1, curTri.e1, curTri.vert0, v3, s03)
            | false, true, false, false -> extend(curTri.vert2, curTri.e2, curTri.vert0, v3, s03)
            | false, false, true, false -> extend(curTri.vert1, e4, v3, curTri.vert0, s03)
            | false, false, false, true -> extend(curTri.vert2, e5, v3, curTri.vert0, s03)
            | true, false, false, true -> between(curTri.e1, e5, s03)
            | false, true, true, false -> between(curTri.e2, e4, s03)
            | true, true, false, false -> between(curTri.e1, curTri.e2, s03)
            | true, false, true, false ->
                if abs s03 < 2.75*dist
                  && min3 curTri.vert1.h curTri.vert0.h v3.h > min e4 curTri.e1
                then
                  between(min curTri.e1 e4, min3 curTri.vert1.h curTri.vert0.h v3.h, nxt s03)
                else
                  -100.0
            | false, true, false, true ->
                if abs s03 < 2.75*dist
                  && min3 curTri.vert2.h curTri.vert0.h v3.h > min e5 curTri.e2
                then
                  between(min curTri.e2 e5, min3 curTri.vert2.h curTri.vert0.h v3.h, nxt s03)
                else
                  -100.0
            | true, true, true, false -> between(curTri.e2, min curTri.e1 e4, s03)
            | true, true, false, true -> between(curTri.e1, min curTri.e2 e5, s03)
            | _,_,_,_ -> printf "Rivers on both e4 and e5 not possible\n"; -100.0

          let subTriangle1: Triangle = {vert0=v3; vert1=curTri.vert0; vert2=curTri.vert1; e0=curTri.e2; e1=e5; e2=e3}
          let subTriangle2: Triangle = {vert0=v3; vert1=curTri.vert0; vert2=curTri.vert2; e0=curTri.e1; e1=e4; e2=e3}
          if isInside currentPosition subTriangle1 then
            curTri <- subTriangle1
          else
            curTri <- subTriangle2

        heights.[i,j] <- (curTri.vert1.h+curTri.vert2.h)/2.0
        river.[i,j] <- river.[i,j] || curTri.e0 <> -100.0 || curTri.e1 <> -100.0 || curTri.e2 <> -100.0

let mutable ctable = [||]

let rec lines2cols lines (c0,r0,g0,b0) =
  match lines with
  | [c;r;g;b] :: lines1 ->
      if c=c0+1 then
        (r,g,b) :: lines2cols lines1 (c,r,g,b)
      else
        List.init (c-c0)
                  (fun i -> (r0+(r-r0)*i/(c-c0),
                             g0+(g-g0)*i/(c-c0),
                             b0+(b-b0)*i/(c-c0)))
        @ lines2cols lines1 (c,r,g,b)
  | _ -> []

let makeColourMap colfile =
  let cf = try new System.IO.StreamReader (colfile + ".col")
           with _ -> failwith "can't open colour file"
  let input = cf.ReadToEnd()
  let _ = cf.Close
  let lines = input.Split([|"\n"|], System.StringSplitOptions.None)
  let lines2 =
        Array.map
          (fun (line:string)
             -> Array.toList
                  (Array.map int
                    (Array.filter
                      (fun s -> s<>"")
                      (line.Split([|" "|], System.StringSplitOptions.None)))))
          lines
  let lines3 = Array.filter (fun l -> l <> []) lines2
  lines2cols (Array.toList lines3) (-1,0,0,0)

let mutable curves = 0.0

// output map as BMP file
let array2bmp nn =
  let height = Array2D.length1 heights
  let width = Array2D.length2 heights
  let ncols = Array.length ctable - 6
  makeBMP.makeBMP nn width height
    (fun (i,j) ->
       // if river.[i,j] && heights.[i,j] >= -0.003 then ctable.[5 + ncols/2]
       if river.[i,j] then ctable.[5]
       else
         if curves>0.0 && heights.[i,j] > 0.0 &&
            i>0 && i<width-1 && j>0 && j<height-1 &&
            (int (heights.[i,j]*curves) > int (heights.[i-1,j]*curves) ||
             int (heights.[i,j]*curves) > int (heights.[i+1,j]*curves) ||
             int (heights.[i,j]*curves) > int (heights.[i,j-1]*curves) ||
             int (heights.[i,j]*curves) > int (heights.[i,j+1]*curves))
         then  ctable.[4 + int (heights.[i,j]*curves) % 2]
         else
           let c = 6 + int (heights.[i,j]*(float ncols)/2.0) + ncols/2
           if c < 6 then ctable.[0]
           else if c >= ncols then ctable.[1]
           else ctable.[c])

// [<EntryPoint>]
let main args =
  let mutable seed = 0.0
  let mutable xmid = 0.5
  let mutable ymid = 0.5
  let mutable scale = 1.0
  let mutable i = 0
  let mutable outfile = "land"
  let mutable colfile = "Olsson2"

  // read command line parameters:
  // -s <float> : set seed
  // -x <float> : set x of centre (between 0.0 and 1.0)
  // -y <float> : set y of centre (between 0.0 and 1.0)
  // -m <float> : set magnification
  // -o <file name> : set output file name (without .bmp extension)
  // -c <file name> : set colour file name (without .col extension)
  // -r : flip river generation (default on)

  while i < Array.length args do
     if args.[i] = "-s" && i+1 < Array.length args
     then seed <- float args.[i+1]
     else if args.[i] = "-x" && i+1 < Array.length args
     then xmid <- max 0.0 (float args.[i+1])
     else if args.[i] = "-y" && i+1 < Array.length args
     then ymid <- max 0.0 (float args.[i+1])
     else if args.[i] = "-m" && i+1 < Array.length args
     then scale <- (float args.[i+1])
     else if args.[i] = "-o" && i+1 < Array.length args
     then outfile <- args.[i+1]
     else if args.[i] = "-c" && i+1 < Array.length args
     then colfile <- args.[i+1]
     else if args.[i] = "-r" then rivers <- 1 - rivers
     else if args.[i] = "-C" && i+1 < Array.length args
     then curves <- (float args.[i+1])
     else ()
     i <- i+1
     
  if seed = 0.0 then
    let ss = 32000
  // let rng = System.Random()
  // let ss = rng.Next(0,1000000)
    seed <- float ss / 100000.0
    //  let rng = System.Random()
    //  let ss = rng.Next(0,1000000)
    //  seed <- float ss / 100000.0
    printf "Seed: %A\n" seed

  ctable <- Array.ofList (makeColourMap colfile)

  let seed00: float = mix seed 3.141593
  let seed01: float = mix seed 2.234551
  let seed10: float = mix seed 5.629339
  let seed11: float = mix seed 9.075109
  let seed55: float = mix seed 7.555155
  let h00: float = between(-0.6,-0.1,seed00)
  let h01: float = between(-0.6,-0.1,seed01)
  let h10: float = between(-0.6,-0.1,seed10)
  let h11: float = between(-0.6,-0.1,seed11)
  let h55: float = between(0.2,0.7,seed55)
  let xmin: float = max 0.0 (xmid - 0.5 / scale)
  let ymin: float = max 0.0 (ymid - 0.5 / scale)
  let xmax: float = min 1.0 (xmid + 0.5 / scale)
  let ymax: float = min 1.0 (ymid + 0.5 / scale)

  triaLoop {x=0.5; y=0.5; s=seed55; h=h55}
       {x=0.0; y=0.0; s=seed00; h=h00}
       {x=0.0; y=1.0; s=seed01; h=h01}
       -100
       -100
       -100
       xmin xmax ymin ymax
       (1.0 / float width / scale);
  triaLoop {x=0.5; y=0.5; s=seed55; h=h55}
       {x=0.0; y=0.0; s=seed00; h=h00}
       {x=1.0; y=0.0; s=seed10; h=h10}
       -100
       -100
       -100
       xmin xmax ymin ymax
       (1.0 / float width / scale);
  triaLoop {x=0.5; y=0.5; s=seed55; h=h55}
       {x=1.0; y=1.0; s=seed11; h=h11}
       {x=0.0; y=1.0; s=seed01; h=h01}
       -100
       -100
       -100
       xmin xmax ymin ymax
       (1.0 / float width / scale);
  triaLoop {x=0.5; y=0.5; s=seed55; h=h55}
       {x=1.0; y=1.0; s=seed11; h=h11}
       {x=1.0; y=0.0; s=seed10; h=h10}
       -100
       -100
       -100
       xmin xmax ymin ymax
       (1.0 / float width / scale);
  
   array2bmp outfile;

   0

main [||]