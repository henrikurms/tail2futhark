fun [int] dropV(int l, [int] x) =
  if 0 <= l then
      if l <= size(0,x) then
          let {v1,v2} = split(l,x) in v2
      else
          empty(int)
  else
      if -l <= size(0,x) then
          let {v1,v2} = split(size(0,x)+l,x) in v1
      else
          empty(int)

fun [int] main() =
  dropV(-7,[1,2,3,4,5,6,7,8,9])
