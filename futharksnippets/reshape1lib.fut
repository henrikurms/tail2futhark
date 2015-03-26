
fun [int] extend(int l, [int] x) =
  reshape((size(0,x) * (l/size(0,x)+1)),replicate(l/size(0,x)+1,x))

fun [int] takeLess(int l, [int] x) =
  let {v1,_} = split((l),x) in v1

fun [int] reshape1int(int l, [int] x) =
  takeLess(l,extend(l,x))

