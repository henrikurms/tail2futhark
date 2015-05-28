
linspace ← { ((⍳⍵)-1) ÷ (⍵-1) }

⍝ Compute pi
m ← 40000
n ← m×m
d ← linspace m
a ← { ⍵ × ⍵ } d
b ← m m ⍴ a
y ← +/+/ (1> (b + (⍉ b)))
pi ← 4×y÷n
⍝ ⎕ ← pi
pi
⍝ 0.1 > | pi - ○ 1
