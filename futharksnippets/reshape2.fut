fun [int] takeV(int l, [int] x) =
  if 0 <= l then
      if l <= size(0,x) then
          let {v1,v2} = split(l,x) in v1
      else
          concat(x,replicate(l - size(0,x),0))
  else
      if -l <= size(0,x) then
          let {v1,v2} = split(size(0,x)+l,x) in v2
      else
          concat(replicate(-(size(0,x)+l),0),x)


fun [int] extend(int l, [int] x) =
  reshape((size(0,x) * (l/size(0,x)+1)),replicate(l/size(0,x)+1,x))

fun [int] takeLess(int l, [int] x) =
  let {v1,_} = split(l,x) in v1

fun [int] reshape1(int l, [int] x) =
  takeLess(l,extend(l,x))

fun [[int]] take2(int l, [[int]] x) =
  reshape((if l <= 0 then (-l) else l,size(1,x)), takeV(l*size(1,x), reshape((size(0,x)*size(1,x)),x)))

fun [[int]] reshape2(int dim1, int dim2, [[int]] x) =
  reshape((dim1,dim2), reshape1(dim1*dim2,reshape((size(0,x)*size(1,x)),x)))

fun [[int]] main() =
  reshape2(4,4,[[1,2,3],[4,5,6]])
