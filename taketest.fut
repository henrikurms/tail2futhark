fun [int] take1_int(int l,[int] x) =
  if (0 <= l) then if (l <= size(0,x)) then let {v1,_} = split((l),x) in
                                            v1 else concat(x,replicate((l - size(0,x)),0)) else if (0 <= (l + size(0,x))) then let {_,v2} = split(((l + size(0,x))),x) in
                                                                                                                               v2 else concat(replicate((l - size(0,x)),0),x)
fun [real] take1_real(int l,[real] x) =
  if (0 <= l) then if (l <= size(0,x)) then let {v1,_} = split((l),x) in
                                            v1 else concat(x,replicate((l - size(0,x)),0.0)) else if (0 <= (l + size(0,x))) then let {_,v2} = split(((l + size(0,x))),x) in
                                                                                                                                 v2 else concat(replicate((l - size(0,x)),0.0),x)
fun [bool] take1_bool(int l,[bool] x) =
  if (0 <= l) then if (l <= size(0,x)) then let {v1,_} = split((l),x) in
                                            v1 else concat(x,replicate((l - size(0,x)),False)) else if (0 <= (l + size(0,x))) then let {_,v2} = split(((l + size(0,x))),x) in
                                                                                                                                   v2 else concat(replicate((l - size(0,x)),False),x)
fun [char] take1_char(int l,[char] x) =
  if (0 <= l) then if (l <= size(0,x)) then let {v1,_} = split((l),x) in
                                            v1 else concat(x,replicate((l - size(0,x)),' ')) else if (0 <= (l + size(0,x))) then let {_,v2} = split(((l + size(0,x))),x) in
                                                                                                                                 v2 else concat(replicate((l - size(0,x)),' '),x)
fun real main() =
  toReal(5)