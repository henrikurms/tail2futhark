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


fun [[int]] take2(int l, [[int]] x) =
  reshape((if l <= 0 then (-l) else l,size(1,x)), takeV(l*size(1,x), reshape((size(0,x)*size(1,x)),x)))

fun [[int]] reshape2(int dim1, int dim2, [[int]] x) =
  reshape((dim1,dim2), takeV(dim1*dim2,reshape((size(0,x)*size(1,x)),x)))

fun [[int]] main() =
  reshape2(2,2,[[1,2,3],[4,5,6]])
