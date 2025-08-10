
-- Colors
let teal : u32 = 0x80FFFF -- change this -- proper river color for nice images
let red : u32 = 0xFF0000 -- 5 to make rivers easily visible for testing purposes

-- water colors
let black6 : u32 = 0x000000 -- 6 black from -1.0 to -0.5 towards darkblue
let darkBlue150 : u32 = 0x0000FF -- 150 darkblue from -0.5 to 0.0 towards lightblue
let lightBlue306 : u32 = 0x80FFFF -- 306 ends at 0.0 - no more 'water' colors.

-- low (positive) altitude colors
let darkGreen307 : u32 = 0x004400 -- 307 evergreen from 0.0 to 0.1 towards 
let darkGreen336 : u32 = 0x226600 -- 336 dark forest green from 0.1 to 0.1675 towards spring green
let darkGreen356 : u32 = 0x228800 -- 356 spring green from 0.1675 to 0.235 towards 
let darkGreen376 : u32 = 0x77AA00 -- 376 chartreuse green from 0.235 to 0.3025 
let darkGreen396 : u32 = 0xBBDD00 -- 396 lime green from 0.3025 to 0.37
let darkGreen416 : u32 = 0xFFBB22 -- 416 golden rod from 0.37 to 0.4375
let darkGreen436 : u32 = 0xEEAA22 -- 436 sunset gold from 0.4375 to 0.505
let darkGreen456 : u32 = 0xDD8822 -- 456 saffron brown end point 0.505 to 0.538
-- medium to high altitude colors
let brown466 : u32 = 0xCC8822 -- 466 bronze yellow from 0.538 to 0.571
let brown476 : u32 = 0xBB6622 -- 476 burnt sienna from 0.571 to 0.604
let brown486 : u32 = 0xAA5522 -- 486 rustic copper from 0.604 to 0.637
let brown496 : u32 = 0x995522 -- 496 Chestnut brown from 0.637 to 0.67
let brown506 : u32 = 0x884422 -- 506 Cedar brown from 0.67 to 0.703
let brown516 : u32 = 0x773322 -- 516 Deep mahogany from 0.703 to 0.736
let brown526 : u32 = 0x553311 -- 526 Dark coco from 0.736 to 0.769
let brown536 : u32 = 0x442200 -- 536 Dark espresso from 0.769 to 0.800
-- high altitude colors
let snow546 : u32 = 0xFFFFFF -- 546 from 0.800 to 1.0
let cloudGrey606 : u32 = 0xB9B9B9 -- 606 end point 1.0

def u32tof32 (c: u32) : (f32, f32, f32) =
  let r = f32.u32 ((c >> 16) & 0xFF)
  let g = f32.u32 ((c >>  8) & 0xFF)
  let b = f32.u32 ((c >>  0) & 0xFF)
  in (r, g, b)

def f32tou32 (r: f32) (g: f32) (b: f32) : u32 =
  let r' = u32.f32 r
  let g' = u32.f32 g
  let b' = u32.f32 b
  in (r' << 16) | (g' << 8) | b'

def lerp_colour (c1: u32) (c2: u32) (t: f32) : u32 =
  let (r1, g1, b1) = u32tof32 c1
  let (r2, g2, b2) = u32tof32 c2
  let r = r1+((r2-r1)*t)
  let g = g1+((g2-g1)*t)
  let b = b1+((b2-b1)*t)
  in f32tou32 r g b

def colours (x : f32) : u32 =
    let c : u32 =
        -- river
        if x <= -3.0f32 then red
        -- water colors
        else if x <= -1.0f32 then black6
        else if x <= -0.5f32 then
            let t = (x + 1.0) / (-0.5 + 1.0)
        in lerp_colour black6 darkBlue150 t
        else if x <= 0.0f32 then
            let t = (x + 0.5) / 0.5 
        in lerp_colour darkBlue150 lightBlue306 t
        -- low altitude colors
        else if x <= 0.1f32 then
            let t = x / 0.1 -- 
        in lerp_colour darkGreen307 darkGreen336 t
        else if x <= 0.1675f32 then
            let t = (x - 0.1) / (0.1675 - 0.1) 
        in lerp_colour darkGreen336 darkGreen356 t
        else if x <= 0.235f32 then
            let t = (x - 0.1675) / (0.235 - 0.1675) 
        in lerp_colour darkGreen356 darkGreen376 t
        else if x <= 0.305 then
            let t = (x - 0.235) / (0.305 - 0.235) 
        in lerp_colour darkGreen376 darkGreen396 t
        else if x <= 0.37 then
            let t = (x - 0.305) / (0.37 - 0.305) 
        in lerp_colour darkGreen396 darkGreen416 t
        else if x <= 0.4375 then
            let t = (x - 0.37) / (0.4375 - 0.37)
        in lerp_colour darkGreen416 darkGreen436 t
        else if x <= 0.505 then
            let t = (x - 0.4375) / (0.505 - 0.4375)
        in lerp_colour darkGreen436 darkGreen456 t
        else if x <= 0.538 then
            let t = (x - 0.505) / (0.538 - 0.505)
        in lerp_colour darkGreen456 brown466 t
        -- medium altitudes
        else if x <= 0.571 then
            let t = (x - 0.538) / (0.571 - 0.538)
        in lerp_colour brown466 brown476 t
        else if x <= 0.604 then
            let t = (x - 0.571) / (0.604 - 0.571)
        in lerp_colour brown476 brown486 t
        else if x <= 0.637 then
            let t = (x - 0.604) / (0.637 - 0.604)
        in lerp_colour brown486 brown496 t
        else if x <= 0.67 then
            let t = (x - 0.637) / (0.67 - 0.637)
        in lerp_colour brown496 brown506 t
        else if x <= 0.703 then
            let t = (x - 0.67) / (0.703 - 0.67)
        in lerp_colour brown506 brown516 t
        else if x <= 0.736 then
            let t = (x - 0.703) / (0.736 - 0.703)
        in lerp_colour brown516 brown526 t
        else if x <= 0.769 then
            let t = (x - 0.736) / (0.769 - 0.736)
        in lerp_colour brown526 brown536 t
        else if x <= 0.8 then
            let t = (x - 0.769) / (0.8 - 0.769)
        in lerp_colour brown536 snow546 t
        -- high altitudes
        else
            let t = (x - 0.8) / (1.0-0.8)
        in lerp_colour snow546 cloudGrey606 (f32.min t 1.0)
    in c

type Vert = {x: f32, y: f32, s: f32, h: f32}

-- benytter triangle fordi det gav mening for mig da jeg brugte stacks
-- ville det være pænere at gå væk fra det? kræver umiddelbart initialisering
-- af temporary variabler inde i hjælper funktionen som alligevel skal holde på de værdier..?
type Triangle = {vert0: Vert, vert1 : Vert, vert2 : Vert, e0 : f32, e1 : f32, e2 : f32}

type Position = {x: f32, y: f32}


let k1 : f32 = 0.32
let k2 : f32 = 0.55

let magic : f32 = 100.0 * f32.pi

def min3(x : f32, y : f32, z : f32) : f32 =
    f32.min x (f32.min y z)

def mix(seed1: f32, seed2: f32) : f32 =
    let a : f32 = (seed1 + magic)
    let b : f32 = (seed2 + magic)
    in (a*b) % 2.0 - 1.0

def nxt(seed : f32) : f32 =
    let a : f32 = (seed + magic) 
    in (a*a) % 2.0 -1.0

def between(a : f32, b : f32, seed : f32) : f32 =
    (a + b + seed*seed*seed*(a-b))/2.0

def distance (p0: Vert, p1: Vert) : f32 =
    let dx : f32 = p1.x - p0.x
    let dy : f32 = p1.y - p0.y  
    in f32.sqrt (dx*dx + dy*dy)

-- blot udskiftet none/some med hhv. -100 eller selve resultatet fra between kald
def extend(v : Vert, e: f32, vNear: Vert, vFar: Vert, s: f32) : f32 =
    if  v.h < f32.min 0.0 e && 0.0 < vFar.h then
        between(e, v.h, s) -- extend down
    else if f32.abs s < 0.65 && e < min3 (v.h, vNear.h, vFar.h) then
        between(e, min3 (v.h, vNear.h, vFar.h), nxt v.s) -- extend up
    else -100.0 -- don't extend

-- følgende to funktioner finder nærmeste subtriangle til et punkt.
-- Tilsvarende til distance bare uden 1023*1023*log(1023) sqrt kald cirka.
def distanceWithoutSqrt(p0: Position) (p1: Position) =
    let dx : f32 = p1.x-p0.x
    let dy : f32 = p1.y-p0.y
    in dx**2+dy**2

let closestSubTriangle (point : Position) (triangle : Triangle) : bool =
    let v1pos = {x=triangle.vert1.x, y=triangle.vert1.y}
    let v2pos = {x=triangle.vert2.x, y=triangle.vert2.y}
    let dv1 = distanceWithoutSqrt point v1pos
    let dv2 = distanceWithoutSqrt point v2pos
    in dv1 < dv2

-- Lavet selve while-løkken om til en hjælpe funktion, da resultatet fra loop bliver binded direkte til funktionen
-- og derfor umiddelbart ikke kan bruges midt inde i en funktion.
-- se https://futhark-lang.org/examples/loops.html
def findSubTriangle (currentPosition : Position, curTri : Triangle, epsilon) : Triangle =
    loop curTri while distance(curTri.vert1, curTri.vert2) >= epsilon do
        let dist  : f32 = distance(curTri.vert1, curTri.vert2)
        let delta : f32 = k1*dist + k2*f32.abs(curTri.vert1.h - curTri.vert2.h)
        let s3 : f32 = mix(curTri.vert1.s, curTri.vert2.s)
        let x3 : f32 = (curTri.vert1.x + curTri.vert2.x) / 2.0
        let y3 : f32 = (curTri.vert1.y + curTri.vert2.y) / 2.0
        -- Bruger bools til match frem for at bruge when i selve case checks, da futhark umiddelbart ikke supporter dette.
        let e0Active = curTri.e0 > -100.0
        let (e4: f32, e5: f32, h3: f32) =
            match e0Active
            case true -> 
                    -- Fjernet tjek med rivers = 1 da der ikke rigtigt er årsag til at kører uden rivers nogenside.
                    if f32.abs(curTri.e0 - curTri.vert1.h) > f32.abs(curTri.e0 - curTri.vert2.h) then
                        (curTri.e0, -100.0, (curTri.e0 + curTri.vert1.h)/2.0+s3*delta)
                    else
                        (-100.0, curTri.e0, (curTri.e0 + curTri.vert2.h)/2.0+s3*delta)
            case _ -> (-100.0, -100.0, (curTri.vert1.h + curTri.vert2.h)/2.0 + s3*delta)
        
        let v3: Vert = {x=x3, y=y3, s=s3, h=h3}
        let s03 : f32 = mix(curTri.vert0.s, s3)

        -- Bruger bools til match frem for at bruge when i selve case checks da futhark umiddelbart ikke supporter dette.
        let e1Active : bool = curTri.e1 > -100.0
        let e2Active : bool = curTri.e2 > -100.0
        let e4Active : bool = e4 > -100.0
        let e5Active : bool = e5 > -100.0

        let e3 : f32 =
            match (e1Active, e2Active, e4Active, e5Active)
            -- one case with 0 surrounding rivers
            case (false, false, false, false) ->
                                if f32.max curTri.vert1.h curTri.vert2.h > 0.1
                                    && f32.min curTri.vert1.h curTri.vert2.h < min3 (curTri.vert0.h, v3.h, -0.1)
                                then between(f32.min curTri.vert1.h curTri.vert2.h, f32.min curTri.vert0.h v3.h, s03)
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
                                if f32.abs s03 < 2.75*dist
                                    && min3(curTri.vert1.h, curTri.vert0.h, v3.h) > f32.min e4 curTri.e1
                                then
                                    between(f32.min curTri.e1 e4, min3 (curTri.vert1.h, curTri.vert0.h, v3.h), nxt s03)
                                else
                                    -100.0
            case (false, true, false, true) ->
                                if f32.abs s03 < 2.75*dist
                                    && min3(curTri.vert2.h, curTri.vert0.h, v3.h) > f32.min e5 curTri.e2
                                then
                                    between(f32.min curTri.e2 e5, min3 (curTri.vert2.h, curTri.vert0.h, v3.h), nxt s03)
                                else
                                    -100.0
            -- two cases with 3 surrounding rivers
            case (true, true, true, false) -> between(curTri.e2, f32.min curTri.e1 e4, s03)
            case (true, true, false, true) -> between(curTri.e1, f32.min curTri.e2 e5, s03)
            -- error case if there are 4 surrounding rivers (e4+e5 not allowed)
            case (_,_,_,_) -> -100.0
        
        let subTriangle1: Triangle = {vert0=v3, vert1=curTri.vert0, vert2=curTri.vert1, e0=curTri.e2, e1=e5, e2=e3}
        let subTriangle2: Triangle = {vert0=v3, vert1=curTri.vert0, vert2=curTri.vert2, e0=curTri.e1, e1=e4, e2=e3}

        in  if closestSubTriangle currentPosition curTri then
                subTriangle1
            else
                subTriangle2

    

-- Den primære funktion som nu kun tager imod en Triangle og epsilon.
-- Har ikke success med inplace updates på hhv. heights og river 2D arrays
-- Gemmer derfor begge værdier i et nyt 2Darray af tuples
-- hvor der bagefter mappes unzip funktionen på således at jeg får en
-- liste med tupler af lister hvorefter jeg kan unzip dette resultat
-- for at få lavet min liste med tupler af lister lavet om til
-- en tuple af 2D arrays, hvor første 2D array består af heights
-- og andet 2D array består af 

-- Mit eget korte eksempel:
-- map unzip [[(0.3, true), (0.2, false)], [(0.1, false), (0.4, true)]]
-- -> [([0.3, 0.2], [true, false]), ([0.1, 0.4], [false, true])]
-- bruger unzip på den ydre liste:
-- -> ([[0.3, 0.2], [0.1, 0.4]], [[true, false], [false, true]])
-- Herfra er blot at dele det ydre pair op i to forskellige variabler.

-- Kan det evt. gøres mere efficient? er unzip parallel?
-- https://futhark-lang.org/examples/inplace.html
-- https://futhark-lang.org/docs/prelude/doc/prelude/zip.html
def triaLoop (originalTriangle : Triangle) (epsilon: f32) (resolution : i64) (xStart : f32) (yStart : f32) (viewDist : f32) =
    map (\i -> 
        map (\j -> 
            let norm_x = (f32.i64 i) / (f32.i64 (resolution-1))
            let norm_y = (f32.i64 j) / (f32.i64 (resolution-1))
            let currentPosition : Position = {
                x = xStart + norm_x * viewDist,
                y = yStart + norm_y * viewDist
            }
            let curTri : Triangle = findSubTriangle(currentPosition, originalTriangle, epsilon)
            let h : f32 = if (curTri.e0 != -100 || curTri.e1 != -100 || curTri.e2 != -100) then -3.0f32 else (curTri.vert1.h + curTri.vert2.h) / 2.0
            let c = colours h
            in c
            ) (iota resolution)
        ) (iota resolution)


def main (resolution : i64) (zoomXStart : f32) (zoomYStart : f32) (viewDistance : f32) =
    let seed = 23000/100000
    let seed0m1 = mix (seed, 1.323564)
    let seed01 = mix (seed, 3.234551)
    let seed21 = mix (seed, 5.86023)
    let h01 = between(-0.2,0.7,seed01)
    let h0m1 = between(-0.6,-0.1,seed0m1)
    let h21 = between(-0.6,-0.1,seed21)
    let someTriangle = {vert0={x=0.0, y=1.0, s=seed01, h=h01}, vert1={x=2.0, y=1.0, s=seed21, h=h21}, vert2={x=0.0, y= -1.0, s=seed0m1, h=h0m1}, e0=(-100), e1=(-100), e2=(-100)}
    let epsilon = (viewDistance/(f32.i64 resolution))
    let heights = triaLoop someTriangle epsilon resolution zoomXStart zoomYStart viewDistance
    in heights

-- > :img main 512i64 0.0f32 0.0f32 1.0f32
