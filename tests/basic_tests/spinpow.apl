⍝ Power Operator example from Legrands "Mastering Dyalog APL", page 415 (first edition).

mat ← 3 3 ⍴ 1 2 3 8 0 4 7 6 5

Spin ← {⊖⍉⍵}

x ← (Spin ⍣ 14) mat

y ← x = Spin Spin mat

∧/∧/y
