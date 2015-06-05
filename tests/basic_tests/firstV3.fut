fun int boolToInt(bool x) =
  if x then 1 else 0
fun int negi(int x) =
  -x
fun real negd(real x) =
  -x
fun int absi(int x) =
  if (x <= 0) then -x else x
fun real absd(real x) =
  if (x <= 0.0) then -x else x
fun int mini(int x,int y) =
  if (x <= y) then x else y
fun real mind(real x,real y) =
  if (x <= y) then x else y
fun int signd(real x) =
  if (0.0 < x)
  then 1
  else if (0.0 == x)
       then 0
       else -1
fun int signi(int x) =
  if (0 < x)
  then 1
  else if (0 == x)
       then 0
       else -1
fun int maxi(int x,int y) =
  if (x <= y) then y else x
fun real maxd(real x,real y) =
  if (x <= y) then y else x
fun bool eqb(bool x,bool y) =
  (!((x || y)) || (x && y))
fun bool xorb(bool x,bool y) =
  (!((x && y)) && (x || y))
fun bool nandb(bool x,bool y) =
  !((x && y))
fun bool norb(bool x,bool y) =
  !((x || y))
fun bool neqi(int x,int y) =
  !((x == y))
fun bool neqd(real x,real y) =
  !((x == y))
fun int resi(int x,int y) =
  if (x == 0)
  then y
  else if ((((y % x) == 0) || ((y > 0) && (x > 0))) || ((y < 0) && (x < 0)))
       then (y % x)
       else (y % (x + x))
fun [int] reshape1_int(int l,[int] x) =
  let roundUp = ((l + (size(0,x) - 1)) / size(0,x)) in
  let extend = reshape(((size(0,x) * roundUp)),replicate(roundUp,x)) in
  let {v1,_} = split((l),extend) in v1
fun [real] reshape1_real(int l,[real] x) =
  let roundUp = ((l + (size(0,x) - 1)) / size(0,x)) in
  let extend = reshape(((size(0,x) * roundUp)),replicate(roundUp,x)) in
  let {v1,_} = split((l),extend) in v1
fun [bool] reshape1_bool(int l,[bool] x) =
  let roundUp = ((l + (size(0,x) - 1)) / size(0,x)) in
  let extend = reshape(((size(0,x) * roundUp)),replicate(roundUp,x)) in
  let {v1,_} = split((l),extend) in v1
fun [char] reshape1_char(int l,[char] x) =
  let roundUp = ((l + (size(0,x) - 1)) / size(0,x)) in
  let extend = reshape(((size(0,x) * roundUp)),replicate(roundUp,x)) in
  let {v1,_} = split((l),extend) in v1
fun [int] take1_int(int l,[int] x) =
  if (0 <= l)
  then if (l <= size(0,x))
       then let {v1,_} = split((l),x) in v1
       else concat(x,replicate((l - size(0,x)),0))
  else if (0 <= (l + size(0,x)))
       then let {_,v2} = split(((l + size(0,x))),x) in v2
       else concat(replicate((l - size(0,x)),0),x)
fun [real] take1_real(int l,[real] x) =
  if (0 <= l)
  then if (l <= size(0,x))
       then let {v1,_} = split((l),x) in v1
       else concat(x,replicate((l - size(0,x)),0.0))
  else if (0 <= (l + size(0,x)))
       then let {_,v2} = split(((l + size(0,x))),x) in v2
       else concat(replicate((l - size(0,x)),0.0),x)
fun [bool] take1_bool(int l,[bool] x) =
  if (0 <= l)
  then if (l <= size(0,x))
       then let {v1,_} = split((l),x) in v1
       else concat(x,replicate((l - size(0,x)),False))
  else if (0 <= (l + size(0,x)))
       then let {_,v2} = split(((l + size(0,x))),x) in v2
       else concat(replicate((l - size(0,x)),False),x)
fun [char] take1_char(int l,[char] x) =
  if (0 <= l)
  then if (l <= size(0,x))
       then let {v1,_} = split((l),x) in v1
       else concat(x,replicate((l - size(0,x)),' '))
  else if (0 <= (l + size(0,x)))
       then let {_,v2} = split(((l + size(0,x))),x) in v2
       else concat(replicate((l - size(0,x)),' '),x)
fun [int] drop1_int(int l,[int] x) =
  if (size(0,x) <= if (l <= 0) then -l else l)
  then empty(int)
  else if (l <= 0)
       then let {v1,_} = split(((l + size(0,x))),x) in v1
       else let {_,v2} = split((l),x) in v2
fun [real] drop1_real(int l,[real] x) =
  if (size(0,x) <= if (l <= 0) then -l else l)
  then empty(real)
  else if (l <= 0)
       then let {v1,_} = split(((l + size(0,x))),x) in v1
       else let {_,v2} = split((l),x) in v2
fun [bool] drop1_bool(int l,[bool] x) =
  if (size(0,x) <= if (l <= 0) then -l else l)
  then empty(bool)
  else if (l <= 0)
       then let {v1,_} = split(((l + size(0,x))),x) in v1
       else let {_,v2} = split((l),x) in v2
fun [char] drop1_char(int l,[char] x) =
  if (size(0,x) <= if (l <= 0) then -l else l)
  then empty(char)
  else if (l <= 0)
       then let {v1,_} = split(((l + size(0,x))),x) in v1
       else let {_,v2} = split((l),x) in v2
fun real main() =
  toFloat(let x = map(fn int (int x) => (x + 1),iota(5)) in x[0])