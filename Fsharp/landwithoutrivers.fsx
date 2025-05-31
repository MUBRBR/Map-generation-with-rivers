#r "makeBMP.dll"

let mutable rivers = 1 // indicates that rivers are made

let width = 1023 // size of bitmap array

let heights = Array2D.create width width -2.0
let river = Array2D.create width width false

let min3 x y z = min x (min y z)
let max3 x y z = max x (max y z)

// magic constant for PRNG
let magic = 100.0 * System.Math.PI

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

let rec tria (v0:Vert) (v1:Vert) (v2:Vert)
             xmin xmax ymin ymax epsilon =
// ei opposite vi
// long edge is e0
// midpoint of e0 is v3
// e4 between v3 and v2
// e5 between v3 and e1
// e3 between v3 and v0
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
    let x3 = (v1.x+v2.x)/2.0
    let y3 = (v1.y+v2.y)/2.0
    let h3 = (v1.h+v2.h)/2.0+s3*delta

    let v3 = {x=x3; y=y3; s=s3; h=h3}

    if dist < epsilon
    then // plot point
      let i = int (round ((x3-xmin) / epsilon))
      let j = int (round ((y3-ymin) / epsilon))
      if i < 0 || i >= width || j < 0 || j >= width
      then ()
      else (heights.[i,j] <-
              if heights.[i,j] = -2.0 then h3 else (heights.[i,j] + h3)/2.0;)
    else // subdivide
      // find attributes for e3
      let s03 = mix v0.s s3 // seed for e3
      
      // recurse on subtriangles
      (tria v3 v0 v1 xmin xmax ymin ymax epsilon;
       tria v3 v0 v2 xmin xmax ymin ymax epsilon)

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
     let rng = System.Random()
     let ss = rng.Next(0,1000000)
     seed <- float ss / 100000.0
     printf "Seed: %A\n" seed

  ctable <- Array.ofList (makeColourMap colfile)

  let seed00 = mix seed 3.141593
  let seed01 = mix seed 2.234551
  let seed10 = mix seed 5.629339
  let seed11 = mix seed 9.075109
  let seed55 = mix seed 7.555155
  let h00 = between(-0.6,-0.1,seed00)
  let h01 = between(-0.6,-0.1,seed01)
  let h10 = between(-0.6,-0.1,seed10)
  let h11 = between(-0.6,-0.1,seed11)
  let h55 = between(0.2,0.7,seed55)
  let xmin = max 0.0 (xmid - 0.5 / scale)
  let ymin = max 0.0 (ymid - 0.5 / scale)
  let xmax = min 1.0 (xmid + 0.5 / scale)
  let ymax = min 1.0 (ymid + 0.5 / scale)

  tria {x=0.5; y=0.5; s=seed55; h=h55}
       {x=0.0; y=0.0; s=seed00; h=h00}
       {x=0.0; y=1.0; s=seed01; h=h01}
       xmin xmax ymin ymax
       (1.0 / float width / scale);

  tria {x=0.5; y=0.5; s=seed55; h=h55}
       {x=0.0; y=0.0; s=seed00; h=h00}
       {x=1.0; y=0.0; s=seed10; h=h10}
       xmin xmax ymin ymax
       (1.0 / float width / scale);

  tria {x=0.5; y=0.5; s=seed55; h=h55}
       {x=1.0; y=1.0; s=seed11; h=h11}
       {x=0.0; y=1.0; s=seed01; h=h01}
       xmin xmax ymin ymax
       (1.0 / float width / scale);

  tria {x=0.5; y=0.5; s=seed55; h=h55}
       {x=1.0; y=1.0; s=seed11; h=h11}
       {x=1.0; y=0.0; s=seed10; h=h10}
       xmin xmax ymin ymax
       (1.0 / float width / scale);

   array2bmp outfile;

   0

main [||]