
fun [int] takeV(int l, [int] x) =
  if 0 <= l then
      if l <= size(0,x) then
          let {v1,v2} = split((l),x) in v1
      else
          concat(x,replicate(l - size(0,x),0))
  else
      if -l <= size(0,x) then
          let {v1,v2} = split((size(0,x)+l),x) in v2
      else
          concat(replicate(l - size(0,x),0),x)


fun int abs(int x) =
  if x <= 0 then -x else x

fun [[int]] take2(int l, [[int]] x) =
  reshape(((abs(l)),size(1,x)), takeV(l*size(1,x), reshape((size(0,x)*size(1,x)),x)))

fun [[int]] main() =
  take2(-2,[[1,2,3],[4,5,6],[7,8,9]])
