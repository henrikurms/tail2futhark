

fun int max(int a, int b) =
  if a < b then b else a

fun int min(int a, int b) =
  if a < b then a else b

fun [int] takeLess(int l, [int] x) =
  let {a,b} = split((l),x) in a

fun [int] takeV(int l, [int] x) =
  takeLess(l,concat(x,replicate(max(0,l-size(0,x)),0))) 

fun [int] old_takeV(int l, [int] x) =
  if 0 <= l then
      if l <= size(0,x) then
          let {v1,v2} = split((l),x) in v1
      else
          concat(x,replicate(l - size(0,x),0))
  else
      if -l <= size(0,x) then
          let {v1,v2} = split((size(0,x)+l),x) in v2
      else
          concat(replicate(-(size(0,x)+l),0),x)


fun [[int]] take2(int l, [[int]] x) =
  reshape(((l*l)/l,size(1,x)), takeV(l*size(1,x), reshape((size(0,x)*size(1,x)),x)))

fun [[int]] main() =
  take2(2,[[1,2,3],[4,5,6],[7,8,9]])
