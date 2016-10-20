⍝ Rotate along the last axis as in (1 ⌽ A)

A ← 3 4 ⍴ ⍳ 12

TA ← ⍉ A                  ⍝ -->  1  5  9 
                          ⍝      2  6 10
                          ⍝      3  7 11
                          ⍝      4  8 12

M6 ← 2 ↑ TA                     ⍝ -->  1  5  9 
                                ⍝      2  6 10

M7 ← ⍉ M6                       ⍝ -->  1  2 
                                ⍝      5  6
                                ⍝      9 10
⎕ ← 6 ⍴ M7

0