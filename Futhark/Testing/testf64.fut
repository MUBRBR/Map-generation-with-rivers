
-- Colors
let red : u32 = 0xFF0000 -- to make rivers easily visible for testing purposes
let teal : u32 = 0x80FFFF -- proper river color for nice images

let lightblue : u32 = 0x80FFFF
let darkblue : u32 = 0x0000FF
let green : u32 = 0x00FF00
let darkgreen : u32 = 0x228822
let lightbrown : u32 = 0xDD8822
let darkbrown : u32 = 0x884422

def u32tof64 (c: u32) : (f64, f64, f64) =
  let r = f64.u32 ((c >> 16) & 0xFF)
  let g = f64.u32 ((c >>  8) & 0xFF)
  let b = f64.u32 ((c >>  0) & 0xFF)
  in (r, g, b)

def f64tou32 (r: f64) (g: f64) (b: f64) : u32 =
  let r' = u32.f64 r
  let g' = u32.f64 g
  let b' = u32.f64 b
  in (r' << 16) | (g' << 8) | b'

def lerp_colour (c1: u32) (c2: u32) (t: f64) : u32 =
  let (r1, g1, b1) = u32tof64 c1
  let (r2, g2, b2) = u32tof64 c2
  let r = r1+((r2-r1)*t)
  let g = g1+((g2-g1)*t)
  let b = b1+((b2-b1)*t)
  in f64tou32 r g b

def colours (x : f64) : u32 =
    let c : u32 =
        if x <= -3.0f64 then red
        else if x <= -0.4f64 then darkblue
        else if x <= 0.0f64 then
            let t = (x + 0.4) / 0.4 -- Normalize height to [0,1] range
        in lerp_colour darkblue lightblue t
        else if x <= 0.3f64 then
            let t = (x - 0.0) / 0.3-0.0 -- Normalize height to [0,1] range
        in lerp_colour darkgreen green t
        else if x <= 0.8f64 then
            let t = (x - 0.3) / (0.8-0.3) -- Normalize height to [0,1] range
        in lerp_colour green lightbrown t
        else
            let t = (x - 0.8) / (1.0-0.8) -- Normalize height to [0,1] range (assuming max height is 1.0)
        in lerp_colour lightbrown darkbrown (f64.min t 1.0) -- Clamp t to max 1.0
    in c


type Vert = {x: f64, y: f64, s: f64, h: f64}

type Triangle = {vert0: Vert, vert1 : Vert, vert2 : Vert, e0 : f64, e1 : f64, e2 : f64}

type Position = {x: f64, y: f64}

let k1 : f64 = 0.32
let k2 : f64 = 0.55

let magic : f64 = 100.0 * f64.pi

def min3(x : f64, y : f64, z : f64) : f64 =
    f64.min x (f64.min y z)

def mix(seed1: f64, seed2: f64) : f64 =
    let a : f64 = (seed1 + magic)
    let b : f64 = (seed2 + magic)
    in (a*b) % 2.0 - 1.0

def nxt(seed : f64) : f64 =
    let a : f64 = (seed + magic) 
    in (a*a) % 2.0 -1.0

def between(a : f64, b : f64, seed : f64) : f64 =
    (a + b + seed*seed*seed*(a-b))/2.0

def distance (p0: Vert, p1: Vert) : f64 =
    let dx : f64 = p1.x - p0.x
    let dy : f64 = p1.y - p0.y
    in f64.sqrt (dx*dx + dy*dy)

def extend(v : Vert, e: f64, vNear: Vert, vFar: Vert, s: f64) : f64 =
    if  v.h < f64.min 0.0 e && 0.0 < vFar.h then
        between(e, v.h, s) -- extend down
    else if f64.abs s < 0.65 && e < min3 (v.h, vNear.h, vFar.h) then
        between(e, min3 (v.h, vNear.h, vFar.h), nxt v.s) -- extend up
    else -100.0 -- don't extend

def distanceWithoutSqrt(p0: Position) (p1: Position) =
    let dx : f64 = p1.x-p0.x
    let dy : f64 = p1.y-p0.y
    in dx**2+dy**2

let closestSubTriangle (point : Position) (triangle : Triangle) : bool =
    let v1pos = {x=triangle.vert1.x, y=triangle.vert1.y}
    let v2pos = {x=triangle.vert2.x, y=triangle.vert2.y}
    let dv1 = distanceWithoutSqrt point v1pos
    let dv2 = distanceWithoutSqrt point v2pos
    in dv1 < dv2

def findSubTriangle (currentPosition : Position, curTri : Triangle, epsilon) : Triangle =
    loop curTri while distance(curTri.vert1, curTri.vert2) >= epsilon do
        let dist  : f64 = distance(curTri.vert1, curTri.vert2)
        let delta : f64 = k1*dist + k2*f64.abs(curTri.vert1.h - curTri.vert2.h)
        let s3 : f64 = mix(curTri.vert1.s, curTri.vert2.s)
        let x3 : f64 = (curTri.vert1.x + curTri.vert2.x) / 2.0
        let y3 : f64 = (curTri.vert1.y + curTri.vert2.y) / 2.0
        let e0Active = curTri.e0 > -100.0
        let (e4: f64, e5: f64, h3: f64) =
            match e0Active
            case true -> 
                    if f64.abs(curTri.e0 - curTri.vert1.h) > f64.abs(curTri.e0 - curTri.vert2.h) then
                        (curTri.e0, -100.0, (curTri.e0 + curTri.vert1.h)/2.0+s3*delta)
                    else
                        (-100.0, curTri.e0, (curTri.e0 + curTri.vert2.h)/2.0+s3*delta)
            case _ -> (-100.0, -100.0, (curTri.vert1.h + curTri.vert2.h)/2.0 + s3*delta)
        
        let v3: Vert = {x=x3, y=y3, s=s3, h=h3}
        let s03 : f64 = mix(curTri.vert0.s, s3)

        let e1Active : bool = curTri.e1 > -100.0
        let e2Active : bool = curTri.e2 > -100.0
        let e4Active : bool = e4 > -100.0
        let e5Active : bool = e5 > -100.0

        let e3 : f64 =
            match (e1Active, e2Active, e4Active, e5Active)
            -- one case with 0 surrounding rivers
            case (false, false, false, false) ->
                                if f64.max curTri.vert1.h curTri.vert2.h > 0.1
                                    && f64.min curTri.vert1.h curTri.vert2.h < min3 (curTri.vert0.h, v3.h, -0.1)
                                then between(f64.min curTri.vert1.h curTri.vert2.h, f64.min curTri.vert0.h v3.h, s03)
                                else -100.0
            -- four cases with 1 surrounding river
            case (true, false, false, false) -> extend(curTri.vert1, curTri.e1, curTri.vert0, v3, s03)
            case (false, true, false, false) -> extend(curTri.vert2, curTri.e2, curTri.vert0, v3, s03)
            case (false, false, true, false) -> extend(curTri.vert1, e4, v3, curTri.vert0, s03)
            case (false, false, false, true) -> extend(curTri.vert2, e5, v3, curTri.vert0, s03)

            -- five cases with 2 surrounding rivers
            case (true, false, false, true) -> between(curTri.e1, e5, s03)
            case (false, true, true, false) -> between(curTri.e2, e4, s03)
            case (true, true, false, false) -> between(curTri.e1, curTri.e2, s03)
            case (true, false, true, false) ->
                                if f64.abs s03 < 2.75*dist
                                    && min3(curTri.vert1.h, curTri.vert0.h, v3.h) > f64.min e4 curTri.e1
                                then
                                    between(f64.min curTri.e1 e4, min3 (curTri.vert1.h, curTri.vert0.h, v3.h), nxt s03)
                                else
                                    -100.0
            case (false, true, false, true) ->
                                if f64.abs s03 < 2.75*dist
                                    && min3(curTri.vert2.h, curTri.vert0.h, v3.h) > f64.min e5 curTri.e2
                                then
                                    between(f64.min curTri.e2 e5, min3 (curTri.vert2.h, curTri.vert0.h, v3.h), nxt s03)
                                else
                                    -100.0
            -- two cases with 3 surrounding rivers
            case (true, true, true, false) -> between(curTri.e2, f64.min curTri.e1 e4, s03)
            case (true, true, false, true) -> between(curTri.e1, f64.min curTri.e2 e5, s03)
            -- error case if there are 4 surrounding rivers (e4+e5 not allowed)
            case (_,_,_,_) -> -100.0
        
        let subTriangle1: Triangle = {vert0=v3, vert1=curTri.vert0, vert2=curTri.vert1, e0=curTri.e2, e1=e5, e2=e3}
        let subTriangle2: Triangle = {vert0=v3, vert1=curTri.vert0, vert2=curTri.vert2, e0=curTri.e1, e1=e4, e2=e3}

        in  if closestSubTriangle currentPosition curTri then
                subTriangle1
            else
                subTriangle2

def triaLoop (originalTriangle : Triangle) (epsilon: f64) (resolution : i64) (xStart : f64) (yStart : f64) (viewDist : f64) =
    map (\i -> 
        map (\j -> 
            let norm_x = (f64.i64 i) / (f64.i64 (resolution-1))
            let norm_y = (f64.i64 j) / (f64.i64 (resolution-1))
            let currentPosition : Position = {
                x = xStart + norm_x * viewDist,
                y = yStart + norm_y * viewDist
            }
            let curTri : Triangle = findSubTriangle(currentPosition, originalTriangle, epsilon)
            let h : f64 = if (curTri.e0 != -100 || curTri.e1 != -100 || curTri.e2 != -100) then -3.0f64 else (curTri.vert1.h + curTri.vert2.h) / 2.0
            let c = colours h
            in c
            ) (iota resolution)
        ) (iota resolution)

-- How quickly can we do triangle calculations
--
-- ==
-- nobench input { 0i64 0.0f64 0.0f64 0.0f64 }
-- compiled input { 512i64 0.0f64 0.0f64 1.0f64 }
-- compiled input { 1024i64 0.0f64 0.0f64 1.0f64 }
-- compiled input { 2048i64 0.0f64 0.0f64 1.0f64 }
-- compiled input { 4096i64 0.0f64 0.0f64 1.0f64 }
-- compiled input { 8192i64 0.0f64 0.0f64 1.0f64 }

def main (resolution : i64) (zoomXStart : f64) (zoomYStart : f64) (viewDistance : f64) =
    let seed = 23000/100000
    let seed0m1 = mix (seed, 1.323564)
    let seed01 = mix (seed, 3.234551)
    let seed21 = mix (seed, 5.86023)
    let h01 = between(-0.2,0.7,seed01)
    let h0m1 = between(-0.6,-0.1,seed0m1)
    let h21 = between(-0.6,-0.1,seed21)
    let someTriangle = {vert0={x=0.0, y=1.0, s=seed01, h=h01}, vert1={x=2.0, y=1.0, s=seed21, h=h21}, vert2={x=0.0, y= -1.0, s=seed0m1, h=h0m1}, e0=(-100), e1=(-100), e2=(-100)}
    let epsilon = (viewDistance/(f64.i64 resolution))
    let heights = triaLoop someTriangle epsilon resolution zoomXStart zoomYStart viewDistance
    in heights


