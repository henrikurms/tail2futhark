fun int boolToInt(bool x) =
  if x then 1 else 0
fun [int] takeLess_int(int l,[int] x) =
  let {v1,_} = split((l),x) in v1
fun [int] reshape1_int(int l,[int] x) =
  takeLess_int(l,extend_int(l,x))
fun [int] extend_int(int l,[int] x) =
  reshape(((size(0,x) * ((l / size(0,x)) + 1))),replicate(((l / size(0,x)) + 1),x))
fun [real] takeLess_real(int l,[real] x) =
  let {v1,_} = split((l),x) in v1
fun [real] reshape1_real(int l,[real] x) =
  takeLess_real(l,extend_real(l,x))
fun [real] extend_real(int l,[real] x) =
  reshape(((size(0,x) * ((l / size(0,x)) + 1))),replicate(((l / size(0,x)) + 1),x))
fun [bool] takeLess_bool(int l,[bool] x) =
  let {v1,_} = split((l),x) in v1
fun [bool] reshape1_bool(int l,[bool] x) =
  takeLess_bool(l,extend_bool(l,x))
fun [bool] extend_bool(int l,[bool] x) =
  reshape(((size(0,x) * ((l / size(0,x)) + 1))),replicate(((l / size(0,x)) + 1),x))
fun [char] takeLess_char(int l,[char] x) =
  let {v1,_} = split((l),x) in v1
fun [char] reshape1_char(int l,[char] x) =
  takeLess_char(l,extend_char(l,x))
fun [char] extend_char(int l,[char] x) =
  reshape(((size(0,x) * ((l / size(0,x)) + 1))),replicate(((l / size(0,x)) + 1),x))
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
       then let {_,v2} = split(((l + size(0,x))),x) in v2
       else let {_,v2} = split((l),x) in v2
fun [real] drop1_real(int l,[real] x) =
  if (size(0,x) <= if (l <= 0) then -l else l)
  then empty(real)
  else if (l <= 0)
       then let {_,v2} = split(((l + size(0,x))),x) in v2
       else let {_,v2} = split((l),x) in v2
fun [bool] drop1_bool(int l,[bool] x) =
  if (size(0,x) <= if (l <= 0) then -l else l)
  then empty(bool)
  else if (l <= 0)
       then let {_,v2} = split(((l + size(0,x))),x) in v2
       else let {_,v2} = split((l),x) in v2
fun [char] drop1_char(int l,[char] x) =
  if (size(0,x) <= if (l <= 0) then -l else l)
  then empty(char)
  else if (l <= 0)
       then let {_,v2} = split(((l + size(0,x))),x) in v2
       else let {_,v2} = split((l),x) in v2
fun real main() =
  let t_v0 = [1,2,3,4,5,6,7,8] in
  let t_v1 = map(fn int (int t_x) => (t_x + 2),t_v0) in
  toFloat(reduce(+,0,t_v1))