
linspace ← { ((⍳⍵)-1) ÷ (⍵-1) }
m ← ⎕ReadIntVecFile 'matrix'
s ← ⎕ReadIntVecFile 'size'
n ← ⊃ s
⍝ a ← (n n ⍴ ÷n)
a ← (n n ⍴ ÷m)
b ← ⍉ a
c ← a +.× b
×/ +/ c

⍝       1  3  5
⍝       2  4  1
⍝
⍝ 1 2   5 11  7  -+->    23
⍝ 3 4  11 25 19  -+->    55
⍝ 5 1   7 19 26  -+->    52
⍝                     65780
