fun real main() =
  let t_v1 = drop1_int(1,map(fn int (int x) => (x + 1),iota(9))) in
  let t_v7 = rearrange((1,0),reshape((8,8),reshape1_int((8 * (8 * 1)),reshape(((size(0,t_v1) * 1)),t_v1)))) in
  let t_v8 = reshape((8,8),reshape1_int((8 * (8 * 1)),reshape(((size(0,t_v1) * 1)),t_v1))) in
  let t_v11 = map(fn [int] ([int] x,[int] y) => map(resi,zip(x,y)),zip(t_v7,t_v8)) in
  let t_v13 = map(fn [bool] ([int] x) => map(fn bool (int t_v12) => (0 == t_v12),x),t_v11) in
  let t_v18 = rearrange((0),map(fn int ([int] x) => reduce(+,0,x),map(fn [int] ([bool] x) => map(boolToInt,x),rearrange((1,0),t_v13)))) in
  let t_v20 = map(fn bool (int t_v19) => (1 == t_v19),t_v18) in
  let t_v24 = reduce(+,0,map(boolToInt,t_v20)) in
  toFloat(t_v24)