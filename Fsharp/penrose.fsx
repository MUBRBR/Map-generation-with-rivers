#r "makeBMP.dll"

let mutable rivers = 1 // indicates that rivers are made

let width = 1023 // size of bitmap array

let heights = Array2D.create width width -2.0
let river = Array2D.create width width false

let min3 x y z = min x (min y z)
let max3 x y z = max x (max y z)

// magic constant for PRNG
let magic = 100.0 * System.Math.PI
let pi = System.Math.PI

// make new seed from two seeds
let mix seed1 seed2 =
  let a = (seed1 + magic)
  let b = (seed2 + magic)
  (a*b) % 2.0 - 1.0

// make new seed from one seed
let nxt seed =
  let a = (seed + magic)
  (a*a) % 2.0 - 1.0
  
// random value between a and b, tend towards middle
let between(a,b,seed) =
  (a + b + seed*seed*seed*(a-b))/2.0

type Vert = { x: float; y: float; s: float; h: float }
type Edge = float option

// distance between points
let distance (p0:Vert) (p1:Vert) =
  let dx = p1.x-p0.x
  let dy = p1.y-p0.y
  sqrt(dx*dx+dy*dy)

let phi = (sqrt(5.0)-1.0)/2.0;
let phi1 = 1.0-phi;

// See https://tartarus.org/~simon/20110412-penrose/penrose.xhtml

// Triangle type 1: 36/72/72 degree angles, v2 at 36-degree angle
// ei opposite vi

let rec tria1 (v0:Vert) (v1:Vert) (v2:Vert)
              (e0:Edge) (e1:Edge) (e2:Edge)
              xmin xmax ymin ymax epsilon =
// v3 splits e0 into e4 and e5 at ratio phi (e4 longest)
// e3 between v0 and v3
// e4 between v2 and v3
// e5 between v1 and v3
  if v0.x < xmin && v1.x < xmin && v2.x < xmin ||
     v0.x > xmax && v1.x > xmax && v2.x > xmax ||
     v0.y < ymin && v1.y < ymin && v2.y < ymin ||
     v0.y > ymax && v1.y > ymax && v2.y > ymax
  then () // outside bounding box
  else
    let dist = distance v1 v2 // distance

    // altitude variation depends on both horisontal and vertical distance
    let delta = 0.32*dist + 0.55*abs(v1.h-v2.h)

    // attributes for v3
    let s3 = mix v1.s v2.s
    let x3 = phi*v1.x+phi1*v2.x
    let y3 = phi*v1.y+phi1*v2.y
    let (e4, e5, h3) // cut long edge, placing river (if any)
       = match e0 with
         | None // no river on e0, v3 height by simple midpoint displacement
             -> (None, None, phi*v1.h+phi1*v2.h + s3*delta)
         | Some h // copy river towards closest-height vertex
              // and v3 height from river height and opposite vertex height
             -> if abs(h-v1.h) > abs(h-v2.h)
                then (e0, None, (h+v1.h)/2.0 + s3*delta)
                else (None, e0, (h+v2.h)/2.0 + s3*delta)
    let v3 = {x=x3; y=y3; s=s3; h=h3}

    if dist < epsilon
    then // plot point
      let i = int (round ((x3-xmin) / epsilon))
      let j = int (round ((y3-ymin) / epsilon))
      if i < 0 || i >= width || j < 0 || j >= width
      then ()
      else (heights.[i,j] <-
              if heights.[i,j] = -2.0 then h3 else (heights.[i,j] + h3)/2.0;
            river.[i,j] <-
              river.[i,j] || e0 <> None || e1 <> None || e2 <> None)
    else // subdivide
      // find attributes for e3
      let s03 = mix v0.s s3 // seed for e3
      let e3 = // determine if river crosses e3
         match (e1, e2, e4, e5) with
         // 1 2 a b
         | (None,None,None,None)
              -> if rivers = 1 && max v1.h v2.h > 0.1
                    && min v1.h v2.h < min3 v0.h v3.h (-0.1)
                 then // make new river
                   Some (between (min v1.h v2.h, min v0.h v3.h, s03))
                 else None

         | (Some e1,None,None,None) -> extend (v1, e1, v0, v3, s03, delta)
         | (None,Some e2,None,None) -> extend (v2, e2, v0, v3, s03, delta)
         | (None,None,Some e4,None) -> extend (v1, e4, v3, v0, s03, delta)
         | (None,None,None,Some e5) -> extend (v2, e5, v3, v0, s03, delta)
         | (Some e1,None,None,Some e5) -> Some (between (e1, e5, s03))
         | (None,Some e2,Some e4,None) -> Some (between (e2, e4, s03))
         | (Some e1,Some e2,None,None) -> Some (between (e1, e2, s03))
         | (Some e1,None,Some e4,None)
              -> if abs s03 < 2.75*dist
                    && min3 v1.h v0.h v3.h > min e4 e1
                 then // make branch
                   Some (between (min e1 e4, min3 v1.h v0.h v3.h, nxt s03))
                 else None
         | (None,Some e2,None,Some e5)
              -> if abs s03 < 2.75*dist
                    && min3 v2.h v0.h v3.h > min e5 e2
                 then // make branch
                   Some (between (min e2 e5, min3 v2.h v0.h v3.h, nxt s03))
                 else None
         | (Some e1,Some e2,Some e4,None)
              -> Some (between (e2, min e1 e4, s03))
         | (Some e1,Some e2,None,Some e5)
              -> Some (between (e1, min e2 e5, s03))
         | (_,_,_,_) -> printf "Rivers on both e4 and e5 not possible\n"; None

      // recurse on subtriangles
      (tria1 v1 v3 v0 e3 e2 e5 xmin xmax ymin ymax epsilon;
       tria2 v3 v2 v0 e1 e3 e4 xmin xmax ymin ymax epsilon)

// Triangle type 2: 36/36/108 degree angles, v0 at 108-degree angle
// ei opposite vi

and tria2 (v0:Vert) (v1:Vert) (v2:Vert)
          (e0:Edge) (e1:Edge) (e2:Edge)
          xmin xmax ymin ymax epsilon =
// v3 splits e0 into e4 and e5 at ratio phi (e4 longest)
// e3 between v0 and v3
// e4 between v2 and v3
// e5 between v1 and v3
  if v0.x < xmin && v1.x < xmin && v2.x < xmin ||
     v0.x > xmax && v1.x > xmax && v2.x > xmax ||
     v0.y < ymin && v1.y < ymin && v2.y < ymin ||
     v0.y > ymax && v1.y > ymax && v2.y > ymax
  then () // outside bounding box
  else
    let dist = distance v1 v2 // distance

    // altitude variation depends on both horisontal and vertical distance
    let delta = 0.32*dist + 0.55*abs(v1.h-v2.h)

    // attributes for v3
    let s3 = mix v1.s v2.s
    let x3 = phi*v1.x+phi1*v2.x
    let y3 = phi*v1.y+phi1*v2.y
    let (e4, e5, h3) // cut long edge, placing river (if any)
       = match e0 with
         | None // no river on e0, v3 height by simple midpoint displacement
             -> (None, None, phi*v1.h+phi1*v2.h + s3*delta)
         | Some h // copy river towards closest-height vertex
              // and v3 height from river height and opposite vertex height
             -> if abs(h-v1.h) > abs(h-v2.h)
                then (e0, None, (h+v1.h)/2.0 + s3*delta)
                else (None, e0, (h+v2.h)/2.0 + s3*delta)
    let v3 = {x=x3; y=y3; s=s3; h=h3}

    if dist < epsilon
    then // plot point
      let i = int (round ((x3-xmin) / epsilon))
      let j = int (round ((y3-ymin) / epsilon))
      if i < 0 || i >= width || j < 0 || j >= width
      then ()
      else (heights.[i,j] <-
              if heights.[i,j] = -2.0 then h3 else (heights.[i,j] + h3)/2.0;
            river.[i,j] <-
              river.[i,j] || e0 <> None || e1 <> None || e2 <> None)
    else // subdivide
      // find attributes for e3
      let s03 = mix v0.s s3 // seed for e3
      let e3 = // determine if river crosses e3
         match (e1, e2, e4, e5) with
         // 1 2 a b
         | (None,None,None,None)
              -> if rivers = 1 && max v1.h v2.h > 0.1
                    && min v1.h v2.h < min3 v0.h v3.h (-0.1)
                 then // make new river
                   Some (between (min v1.h v2.h, min v0.h v3.h, s03))
                 else None

         | (Some e1,None,None,None) -> extend (v1, e1, v0, v3, s03, delta)
         | (None,Some e2,None,None) -> extend (v2, e2, v0, v3, s03, delta)
         | (None,None,Some e4,None) -> extend (v1, e4, v3, v0, s03, delta)
         | (None,None,None,Some e5) -> extend (v2, e5, v3, v0, s03, delta)
         | (Some e1,None,None,Some e5) -> Some (between (e1, e5, s03))
         | (None,Some e2,Some e4,None) -> Some (between (e2, e4, s03))
         | (Some e1,Some e2,None,None) -> Some (between (e1, e2, s03))
         | (Some e1,None,Some e4,None)
              -> if abs s03 < 2.75*dist
                    && min3 v1.h v0.h v3.h > min e4 e1
                 then // make branch
                   Some (between (min e1 e4, min3 v1.h v0.h v3.h, nxt s03))
                 else None
         | (None,Some e2,None,Some e5)
              -> if abs s03 < 2.75*dist
                    && min3 v2.h v0.h v3.h > min e5 e2
                 then // make branch
                   Some (between (min e2 e5, min3 v2.h v0.h v3.h, nxt s03))
                 else None
         | (Some e1,Some e2,Some e4,None)
              -> Some (between (e2, min e1 e4, s03))
         | (Some e1,Some e2,None,Some e5)
              -> Some (between (e1, min e2 e5, s03))
         | (_,_,_,_) -> printf "Rivers on both e4 and e5 not possible\n"; None

      // recurse on subtriangles
      (tria1 v3 v0 v2 e1 e4 e3 xmin xmax ymin ymax epsilon;
       tria2 v3 v0 v1 e2 e5 e3 xmin xmax ymin ymax epsilon)

// possibly extend river
and extend(v, e, vNear, vFar, s, delta) =
  if  v.h < min 0.0 e && 0.0 < vFar.h then
    Some (between (e, v.h, s)) // extend down
  else if abs s < 0.65 && e < min3 v.h vNear.h vFar.h then
    Some (between (e, min3 v.h vNear.h vFar.h, nxt v.s)) // extend up
  else None // don't extend

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
       if river.[i,j] && heights.[i,j] >= -0.003 then ctable.[5 + ncols/2]
       // if river.[i,j] then ctable.[5]
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
  let mutable outfile = "penrose"
  let mutable colfile = "Olsson2"
  // let mutable colfile = "OlssonW"

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
     let rng = System.Random()
     let ss = rng.Next(0,1000000)
     seed <- float ss / 100000.0
     printf "Seed: %A\n" seed

  ctable <- Array.ofList (makeColourMap colfile)

  let seed00 = mix seed 3.141593
  let seed01 = mix seed 2.234551
  let seed10 = mix seed 5.629339
  let seed11 = mix seed 9.075109
  let h00 = between(-0.4,-0.1,0.7*seed00)
  let h01 = between(-0.4,-0.1,0.7*seed01)
  let h10 = between(0.1,0.3,0.7*seed10)
  let h11 = between(0.3,0.7,0.7*seed11)
  let xmin = max 0.0 (xmid - 0.5 / scale)
  let ymin = max 0.0 (ymid - 0.5 / scale)
  let xmax = min 1.0 (xmid + 0.5 / scale)
  let ymax = min 1.0 (ymid + 0.5 / scale)
  let cs = cos(36.0*pi/180.0)
  let sn = sin(36.0*pi/180.0)
  let cs1 = 0.5 * cs / sn
  let w = 0.5 / sn

  tria1 {x = cs1; y = 1.0; s = seed11; h = h11}
        {x = w; y = 0.5; s = seed10; h = h10}
        {x = 0.0; y = 0.5; s=seed00; h = h00}
        None
        None
        None
        xmin xmax ymin ymax
        (1.0 / float width / scale);

  tria1 {x = cs1; y = 0.0; s = seed01; h = h01}
        {x = w; y = 0.5; s = seed10; h = h10}
        {x = 0.0; y = 0.5; s=seed00; h = h00}
        None
        None
        None
        xmin xmax ymin ymax
        (1.0 / float width / scale);

   array2bmp outfile;

   0


main [||]