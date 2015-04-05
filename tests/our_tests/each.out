fun real main() =
  let t_v1 = reshape((2,2),reshape1_int((2 * (2 * 1)),reshape(((size(0,[1,2,3,4]) * 1)),[1,2,3,4]))) in
  let t_v3 = map(fn [int] ([int] x) => map(fn int (int t_v2) => (t_v2 + 5),x),t_v1) in
  let t_v4 = reshape((4),reshape1_int((4 * 1),reshape(((size(0,t_v3) * (size(1,t_v3) * 1))),t_v3))) in
  let t_v7 = reduce(+,0,t_v4) in
  toReal(t_v7)