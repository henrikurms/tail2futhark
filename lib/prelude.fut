-------------------------------------------------------------------
-- This Futhark program is generated automatically by tail2futhark.
-------------------------------------------------------------------

import "/futlib/math"
import "/futlib/array"

let radix_sort_step_up [n] (p:([n]u32,[n]i32), digit_n:i32) : ([n]u32,[n]i32) =
  let (xs,is)    = p
  let bits       = map (\(x:u32):i32 -> (i32.u32 x >>> digit_n) & 1) xs
  let bits_inv   = map (\(b:i32):i32 -> 1 - b) bits
  let ps0        = scan (+) 0 bits_inv
  let ps0_clean  = map2 (*) bits_inv ps0
  let ps1        = scan (+) 0 bits
  let ps0_offset = reduce (+) 0 bits_inv
  let ps1_clean  = map (\(i:i32) : i32 -> i+ps0_offset) ps1
  let ps1_clean' = map2 (*) bits ps1_clean
  let ps         = map2 (+) ps0_clean ps1_clean'
  let ps_actual  = map (\(p:i32) : i32 -> p - 1) ps
  in (scatter (copy(xs)) ps_actual xs,
      scatter (copy(is)) ps_actual is)

let radix_sort_step_down [n] (p:([n]u32,[n]i32), digit_n:i32) : ([n]u32,[n]i32) =
  let (xs,is)    = p
  let bits       = map (\(x:u32):i32 -> (i32.u32 x >>> digit_n) & 1) xs
  let bits_inv   = map (\(b:i32):i32 -> 1 - b) bits
  let ps1        = scan (+) 0 bits
  let ps1_offset = reduce (+) 0 bits
  let ps1_clean  = map2 (*) bits ps1
  let ps0        = scan (+) 0 bits_inv
  let ps0_clean  = map (\(i:i32) : i32 -> i+ps1_offset) ps0
  let ps0_clean' = map2 (*) bits_inv ps0_clean
  let ps         = map2 (+) ps1_clean ps0_clean'
  let ps_actual  = map (\(p:i32) : i32 -> p - 1) ps
  in (scatter (copy(xs)) ps_actual xs,
      scatter (copy(is)) ps_actual is)

let radix_sort_up [n] (xs: [n]u32) : ([n]u32,[n]i32) =
  let is = iota(n) in
  let is = map (+1) is in
  loop (p:([n]u32,[n]i32)) = (xs,is) for i < 32 do
       radix_sort_step_up(p,i)

let radix_sort_down [n] (xs: [n]u32) : ([n]u32,[n]i32) =
  let is = iota(n) in
  let is = map (+1) is in
  loop (p:([n]u32,[n]i32)) = (xs,is) for i < 32 do
    radix_sort_step_down(p,i)

let grade_up [n] (xs: [n]i32) : [n]i32 =
  let xs = map u32.i32 xs in
  let (_,is) = radix_sort_up xs in is

let grade_down [n] (xs: [n]i32) : [n]i32 =
  let xs = map u32.i32 xs in
  let (_,is) = radix_sort_down xs in is

let sgmScanSum [n] (vals:[n]i32) (flags:[n]bool) : [n]i32 =
  let pairs = scan ( \((v1,f1):(i32,bool)) ((v2,f2):(i32,bool)) : (i32,bool) ->
                       let f = f1 || f2
                       let v = if f2 then v2 else v1+v2
                       in (v,f) ) (0,false) (zip vals flags)
  let (res,_) = unzip pairs
  in res

let replIdx [n] (reps:[n]i32) : []i32 =
  let tmp = scan (+) 0 reps
  let sers = map (\(i:i32):i32 -> if i == 0 then 0 else unsafe tmp[i-1]) (iota(n))
  let m = unsafe tmp[n-1]
  let tmp2 = scatter (replicate m 0) sers (iota(n))
  let flags = map (>0) tmp2
  let res = sgmScanSum tmp2 flags
  in res

let negi (x: i32): i32 =
  -x

let absi (x: i32): i32 =
  if x <= 0
  then -x
  else x

let mini (x: i32) (y: i32): i32 =
  if x <= y
  then x
  else y

let maxi (x: i32) (y: i32): i32 =
  if x <= y
  then y
  else x

let eqb (x: bool) (y: bool): bool =
  ! (x || y) || (x && y)

let xorb (x: bool) (y: bool): bool =
  ! (x && y) && (x || y)

let nandb (x: bool) (y: bool): bool =
  ! (x && y)

let norb (x: bool) (y: bool): bool =
  ! (x || y)

let neqi (x: i32) (y: i32): bool =
  x != y

let neqd (x: f32) (y: f32): bool =
  x != y

let resi (x: i32) (y: i32): i32 =
  if x == 0
  then y
  else (y % x)

let frotate 't (i: i32) (xs: []t) = rotate i xs
