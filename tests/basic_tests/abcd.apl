
elements ← 'ABCD'

samples ← 8 6 ⍴ 'CCCAACBAADABCAACDCACDDBCDDCCCBADCBCCCACCCBCCBACC'

X ← elements ∘.= samples

Y ← ∨/ X
⍝ ⎕← 'Result:'
R← ∧⌿∨/ X            ⍝ --> [8](0,0,0,1,0,1,0,0)

T ← ∧/R=0 0 0 1 0 1 0 0

⍝ {∧/⍵}¨∨/{⍵ = elements}¨samples

⍝ T

T
