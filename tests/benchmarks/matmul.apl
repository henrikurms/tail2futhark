
linspace ← { ((⍳⍵)-1) ÷ (⍵-1) }

m ← ⎕ReadIntVecFile 'matrix'
s ← ⎕ReadIntVecFile 'size'
n ← ⊃ s
⍝ a ← (n n ⍴ ÷n)
a ← (n n ⍴ ÷m)
b ← ⍉ a
c ← a +.× b
×/ +/ c
