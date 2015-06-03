
linspace ← { ((⍳⍵)-1) ÷ (⍵-1) }

⍝ Compute pi
m ← 13722
n ← m×m
a ← linspace m
b ← (n 1 ⍴ (m m ⍴ a)) , (n 1 ⍴ ⍉ (m m ⍴ a))
x ← +/1>+/b*2
pi ← 4×x÷n
⍝ ⎕ ← pi
pi
⍝ 0.1 > | pi - ○ 1
