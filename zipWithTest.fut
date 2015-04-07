fun [int] main() =
  let v0 = [1,2,3] in
  let v1 = [4,5,6] in 
  let v2 = map(+,zip(v0,v1)) in
  v2
