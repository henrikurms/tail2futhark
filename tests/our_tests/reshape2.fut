fun real main() =
  let t_v1 = reshape((3,3),reshape1_int((3 * (3 * 1)),reshape(((size(0,[4,5]) * 1)),[4,5]))) in
  let t_v2 = reshape((4,4),reshape1_int((4 * (4 * 1)),reshape(((size(0,t_v1) * (size(1,t_v1) * 1))),t_v1))) in
  toReal(reduce(+,0,[size(0,t_v2),size(1,t_v2)]))