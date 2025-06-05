-- distance
-- between
-- mix
-- magic
-- min / min3 / max
-- extend (river)

-- round
-- abs
-- sqrt


-- types: 
-- type Vert = { x: float; y: float; s: float; h: float }
-- type Edge = float option


type vert = {x: f64, y: f64, s: f64, h: f64}
-- def vert_record : {x: f32, y: f32, s: f32, h: f32} =
-- type vert = #vertice {x: f32; y: f32; s: f32; h: f32}
-- type Edge = {x: f32}
type edge 'a = #some a | #none

-- i stedet for option type så bare vælg meget lav eller meget høj værdi.

-- Fiks det i fsharp med pixelsøgning og uden floder før futhark.
-- beskriv hele processen i rapporten.

-- let min3 (x : 'a) (y : 'a) (z : a') : 'a =
--     min(x min(y z))

let width : i64 = 1023

let heights 

let magic : f64 = 100.0 * f64.pi

def between(a : f64, b : f64, seed : f64) : f64 =
    (a + b + seed*seed*seed*(a-b))/2.0

def distance (p0: vert, p1: vert) : f64 =
    let dx : f64 = p1.x - p0.x
    let dy : f64 = p1.y - p0.y
    in f64.sqrt (dx*dx + dy*dy)

def mix(seed1: f64, seed2: f64) : f64 =
    let a : f64 = (seed1 + magic)
    let b : f64 = (seed2 + magic)
    in (a*b) % 2.0 - 1.0
    -- in (a*b) %% 2.0 - 1.0 -- double % for more efficient but rounds towards 0

def outsidebounds (v0: vert, v1: vert, v2: vert, xmin: f64, xmax: f64, ymin: f64, ymax: f64) : bool =
    -- if ((v0.x < xmin && v1.x < xmin && v2.x < xmin) || (v0 > xmax && v1 > xmax && v2 > xmax)) then
    --     true
    -- else
    --     false
    if (v0.x < xmin && v1.x < xmin && v2.x < xmin ||
        v0.x > xmax && v1.x > xmax && v2.x > xmax ||
        v0.y < ymin && v1.y < ymin && v2.y < ymin ||
        v0.y > ymax && v1.y > ymax && v2.y > ymax)
        then true
    else
        false

def tria (v0: vert) (v1: vert) (v2: vert) (xmin: f64) (xma: f64) (ymin: f64) (epsilon: f64) =
    -- if (v0.x < xmin && v1.x < xmin && v2.x < xmin ||
    --     v0.x > xmax && v1.x > xmax && v2.x > xmax ||
    --     v0.y < ymin && v1.y < ymin && v2.y < ymin ||
    --     v0.y > ymax && v1.y > ymax && v2.y > ymax)
    if (outsidebounds(v0, v1, v2, xmin, xmax, ymin, ymax))
    then ()
    else
        let dist: f64 = distance(v1, v2)
        -- altitude variation depends on both horisontal and vertical distance

        let delta : f64 = 0.32*dist + 0.55*f64.abs(v1.h-v2.h)
        
        -- attributes for v3
        let s3 : f64 = mix(v1.s, v2.s)
        let x3 : f64 = (v1.x+v2.x)/2.0
        let y3 : f64 = (v1.y+v2.y)/2.0


def main =
    let e : edge = #none
    in e


-- fjern alt med edges få at gøre det uden floder.
-- benyt at blot tjekke om punkt er tættest på et hjørne eller det andet.

-- width = 2^n hvilket giver log(2^n)*2 udregninger.



-- def main =
--     magic
-- def main =
--     let a : f64 = 3.0
--     let b : f64 = 5.0
--     let seed : f64 = 1.5
--     in between(a, b, seed)
-- def main =
--     let test1 : vert = {x = 10.0, y = 50.0, s = 10.0, h = 10.0}
--     -- let test1 : vert = (10.0, 10.0, 10.0, 10.0)
--     let test2 : vert = {x = 20.0, y = 30.0, s = 20.0, h = 20.0}
--     -- let test2 : vert = (20.0, 20.0, 20.0, 20.0)
--     in distance (test1) (test2)