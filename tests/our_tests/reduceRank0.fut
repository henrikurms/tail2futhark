fun real main() =
  let t_v0 = [1,2,3,4,5,6] in
  let t_v1 = reduce(op +,0,t_v0) in
  toReal(t_v1)