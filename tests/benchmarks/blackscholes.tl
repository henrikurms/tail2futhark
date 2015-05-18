let v5:[double]0 = divd(addd(ln(divd(60.00,65.00)),muld(addd(0.10,divd(powd(0.20,2.00),2.00)),1.00)),muld(0.20,powd(1.00,0.50))) in
let v6:[double]0 = subd(v5,muld(0.20,powd(1.00,0.50))) in
let v8:[double]1 = [0.32,~0.36,1.78,~1.82,1.33] in
let v9:[double]0 = absd(v6) in
let v10:[double]0 = divd(1.00,addd(1.00,muld(0.23,v9))) in
let v17:[double]1 = reshape([5],v8) in
let v23:[double]1 = reshape([5],eachV(fn v12:[double]0 => powd(v10,v12),eachV(i2d,iotaV(5)))) in
let v28:[double]0 = subd(1.00,muld(divd(1.00,powd(muld(2.00,muld(1.00,pi)),0.50)),muld(powd(2.72,negd(divd(muld(v9,v9),2.00))),reduce(addd,0.00,zipWith(muld,v17,v23))))) in
let v30:[double]1 = [0.32,~0.36,1.78,~1.82,1.33] in
let v31:[double]0 = absd(v5) in
let v32:[double]0 = divd(1.00,addd(1.00,muld(0.23,v31))) in
let v39:[double]1 = reshape([5],v30) in
let v45:[double]1 = reshape([5],eachV(fn v34:[double]0 => powd(v32,v34),eachV(i2d,iotaV(5)))) in
let v50:[double]0 = subd(1.00,muld(divd(1.00,powd(muld(2.00,muld(1.00,pi)),0.50)),muld(powd(2.72,negd(divd(muld(v31,v31),2.00))),reduce(addd,0.00,zipWith(muld,v39,v45))))) in
let v51:[double]0 = subd(muld(60.00,addd(muld(i2d(absi(mini(0,signd(v5)))),subd(1.00,v50)),muld(i2d(subi(1,absi(mini(0,signd(v5))))),v50))),muld(muld(65.00,powd(2.72,negd(muld(0.10,1.00)))),addd(muld(i2d(absi(mini(0,signd(v6)))),subd(1.00,v28)),muld(i2d(subi(1,absi(mini(0,signd(v6))))),v28)))) in
v51
