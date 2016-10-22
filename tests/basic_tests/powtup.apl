
f ← {
  a1 ← ⍵[1]
  a2 ← ⍵[2]
  a3 ← ⍵[3]
  r1 ← (1 1) + a1
  r2 ← a2,'hej'
  (r1 r2 3)
}

res ← (f⍣3) ((2 3) 'asbc' 8)
⍝ res ← f (2 '')

T1 ← ∧/res[1]=5 6
T2 ← ∧/res[2]='asbchejhejhej'
T3 ← res[3]=3

∧/T1 T2 T3