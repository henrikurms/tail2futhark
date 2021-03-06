⍝ Rotate along the last axis as in (1 ⌽ A)

V ← ⍳5
A ← 3 4 ⍴ ⍳ 12

TA ← ⍉ A                  ⍝ -->  1  5  9 
                          ⍝      2  6 10
                          ⍝      3  7 11
                          ⍝      4  8 12
V1 ← 1 ⌽ V
T1 ← ∧/V1=2 3 4 5 1

V2 ← ¯1 ⌽ V
T2 ← ∧/V2=5 1 2 3 4

A3 ← 1 ⌽ A
T3 ← ∧/∧/A3=3 4 ⍴ 2 3 4 1 6 7 8 5 10 11 12 9
T4 ← ∧/(+/A3)=10 26 42

rot ← { (⍉ ⍺ ↓ ⍉ ⍵) , ⍉ ⍺ ↑ ⍉ ⍵ } 
X5 ← 1 rot A           ⍝ -->   2  3  4  1
                       ⍝       6  7  8  5
                       ⍝      10 11 12  9
V5 ← +/ X5             ⍝ --> [3](10,26,42)
T5 ← ∧/V5=10 26 42

M6 ← 2 ↑ TA                     ⍝ -->  1  5  9 
                                ⍝      2  6 10
T6 ← ∧/∧/M6=2 3 ⍴ 1 5 9 2 6 10

M7 ← ⍉ M6                       ⍝ -->  1  2 
                                ⍝      5  6
                                ⍝      9 10
T7 ← ∧/∧/M7=3 2 ⍴ 1 2 5 6 9 10

+ / T1 T2 T3 T4 T5 T6 T7